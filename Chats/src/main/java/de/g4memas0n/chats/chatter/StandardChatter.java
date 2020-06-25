package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.event.chatter.ChatterFocusChangedEvent;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.logging.Log;
import de.g4memas0n.chats.util.type.ChannelType;
import de.g4memas0n.chats.util.type.InfoType;
import de.g4memas0n.chats.util.type.ModifyType;
import de.g4memas0n.chats.util.type.StorageType;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * Implementation of a standard currently online chatter.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: August 6th, 2019
 * changed: June 22th, 2020
 */
public class StandardChatter implements IChatter {

    private final IChats instance;
    private final Player player;
    private final IStorageFile storage;

    private final Set<IChannel> channels;

    private Set<UUID> ignores;

    private IChannel focused;
    private IChannel lastFocused;
    private IChannel lastPersist;
    private UUID lastPartner;

    private boolean muted;
    private boolean socialSpy;

    private Future<?> saveTask;

    protected StandardChatter(@NotNull final IChats instance,
                              @NotNull final Player player,
                              @NotNull final IStorageFile storage) {
        this.instance = instance;
        this.player = player;
        this.storage = storage;

        this.channels = new HashSet<>();
        this.ignores = new HashSet<>();
    }

    @Override
    public @NotNull IStorageFile getStorage() {
        return this.storage;
    }

    @Override
    public synchronized void delete() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(true);
        }

        try {
            this.storage.delete();

            Log.getPlugin().debug(String.format("Deleted storage file '%s' of chatter: %s",
                    this.storage.getFile().getName(), this.player.getName()));
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to delete storage file '%s' of chatter '%s': %s",
                    this.storage.getFile().getName(), this.player.getName(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            Log.getPlugin().debug(String.format("Loaded chatter '%s' from storage file: %s",
                    this.player.getName(), this.storage.getFile().getName()));
        } catch (MissingStorageFileException ex) {
            Log.getPlugin().warning(String.format("Unable to find storage file '%s' of chatter '%s'. Loading "
                            + "default configuration...", this.storage.getFile().getName(), this.player.getName()));

            this.storage.clear();
            this._delayedSave();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPlugin().warning(String.format("Unable to load storage file '%s' of chatter '%s'. Loading "
                            + "previous configuration...", this.storage.getFile().getName(), this.player.getName()));
            Log.getPlugin().debug(String.format("Caused by %s: %s", ex.getClass(), ex.getMessage()));
        }

        if (!this.player.getUniqueId().equals(this._getUniqueId())) {
            Log.getPlugin().warning(String.format("Detected invalid unique-id in storage file '%s' of chatter: %s",
                    this.storage.getFile().getName(), this.player.getName()));

            this._setUniqueId(this.player.getUniqueId());
            this._delayedSave();
        }

        if (!this.player.getName().equals(this._getName())) {
            this._setName(this.player.getName());
            this._delayedSave();
        }

        if (!this.channels.isEmpty()) {
            this.channels.forEach(channel -> channel.setMember(this, false));
            this.channels.clear();
        }

        this.channels.addAll(this._getChannels());
        this.channels.add(this.instance.getChannelManager().getDefault());

        this.focused = this._getFocus();

        if (!this.channels.contains(this.focused)) {
            this.channels.add(this.focused);
            this._setChannels(this.channels);
            this._delayedSave();
        }

        this.lastFocused = null;
        this.lastPersist = null;
        this.lastPartner = null;

        this.muted = this._getMuted();
        this.socialSpy = this._getSocialSpy();
        this.ignores = this._getIgnores();

        this.channels.forEach(channel -> channel.setMember(this, true));
    }

    @Override
    public synchronized void save() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(false);
        }

        if (!this.storage.getFile().exists()) {
            this._setUniqueId(this.player.getUniqueId());
            this._setName(this.player.getName());
            this._setChannels(this.channels);
            this._setFocus(this.focused);
            this._setMuted(this.muted);
            this._setSocialSpy(this.socialSpy);
            this._setIgnores(this.ignores);
        }

        this.storage.set("last.played", this.getLastPlayed());

        try {
            this.storage.save();

            Log.getPlugin().debug(String.format("Saved chatter '%s' to storage file: %s",
                    this.player.getName(), this.storage.getFile().getName()));
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to save storage file '%s' of chatter '%s': %s",
                    this.storage.getFile().getName(), this.player.getName(), ex.getMessage()));
        }
    }

    protected synchronized final void _delayedSave() {
        if (!this.instance.getSettings().isAutoSave()) {
            return;
        }

        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            return;
        }

        this.saveTask = this.instance.scheduleStorageTask(this::save);
    }

    @Override
    public final @NotNull Player getPlayer() {
        return this.player;
    }

    protected final @Nullable UUID _getUniqueId() {
        if (!this.storage.contains("uuid")) {
            this._setUniqueId(this.player.getUniqueId());
            this._delayedSave();
        }

        return this.storage.getUniqueId("uuid");
    }

    @Override
    public final @NotNull UUID getUniqueId() {
        return this.player.getUniqueId();
    }

    protected final void _setUniqueId(@NotNull final UUID uniqueId) {
        this.storage.set("uuid", uniqueId.toString());
    }

    protected final @Nullable String _getName() {
        if (!this.storage.contains("last.name")) {
            this._setName(this.player.getName());
            this._delayedSave();
        }

        return this.storage.getString("last.name");
    }

    @Override
    public final @NotNull String getName() {
        return this.player.getName();
    }

    protected final void _setName(@NotNull final String name) {
        this.storage.set("last.name", name);
    }

    @Override
    public final @NotNull String getDisplayName() {
        return this.player.getDisplayName();
    }

    @Override
    public final long getLastPlayed() {
        return System.currentTimeMillis();
    }

    protected final boolean _getMuted() {
        if (!this.storage.contains("muted")) {
            this._setMuted(false);
            this._delayedSave();
        }

        return this.storage.getBoolean("muted", false);
    }

    @Override
    public boolean isMuted() {
        return muted;
    }

    @Override
    public boolean setMuted(final boolean muted) {
        if (muted == this.muted) {
            return false;
        }

        this.muted = muted;
        this._setMuted(muted);
        this._delayedSave();
        return true;
    }

    protected final void _setMuted(final boolean muted) {
        this.storage.set("muted", muted);
    }

    protected final boolean _getSocialSpy() {
        if (!this.storage.contains("social-spy")) {
            this._setSocialSpy(false);
            this._delayedSave();
        }

        if (this.storage.getBoolean("social-spy", false)) {
            if (this.player.hasPermission(Permission.SOCIAL_SPY.getNode())) {
                return true;
            }

            this._setSocialSpy(false);
            this._delayedSave();
        }

        return false;
    }

    @Override
    public synchronized boolean isSocialSpy() {
        return this.socialSpy;
    }

    @Override
    public synchronized boolean setSocialSpy(final boolean enabled) {
        if (enabled == this.socialSpy) {
            return false;
        }

        this.socialSpy = enabled;
        this._setSocialSpy(enabled);
        this._delayedSave();
        return true;
    }

    protected final void _setSocialSpy(final boolean enabled) {
        this.storage.set("social-spy", enabled);
    }

    // Active Channel Methods:
    protected final @NotNull IChannel _getFocus() {
        if (!this.storage.contains("focus")) {
            this._setFocus(this.instance.getChannelManager().getDefault());
            this._delayedSave();
        }

        final String name = this.storage.getString("focus");

        if (name != null && !name.isEmpty()) {
            final IChannel persist = this.instance.getChannelManager().getPersist(name);

            if (persist != null) {
                return persist;
            }
        }

        return this.instance.getChannelManager().getDefault();
    }

    @Override
    public synchronized @NotNull IChannel getFocus() {
        if (this.focused == null) {
            return this.instance.getChannelManager().getDefault();
        }

        return this.focused;
    }

    @Override
    public synchronized boolean setFocus(@Nullable final IChannel channel) {
        if (channel == null) {
            return this.setFocus(this.instance.getChannelManager().getDefault());
        }

        if (channel.equals(this.focused)) {
            return false;
        }

        if (!this.channels.contains(channel)) {
            this.joinChannel(channel);
        }

        if (this.focused != null) {
            if (this.channels.contains(this.focused)) {
                if (!this.focused.isConversation()) {
                    this.lastFocused = this.focused;

                    if (this.focused.isPersist()) {
                        this.lastPersist = this.focused;
                    }
                }
            }

            final Event event = new ChatterFocusChangedEvent(this, this.focused);

            this.focused = channel;
            this.instance.getServer().getPluginManager().callEvent(event);
        } else {
            this.focused = channel;
        }

        if (channel.isPersist()) {
            this._setFocus(channel);
            this._delayedSave();
        }

        return true;
    }

    protected final void _setFocus(@NotNull final IChannel focus) {
        this.storage.set("focus", focus.getFullName());
    }

    // Last Sources Methods:
    @Override
    public synchronized @Nullable IChannel getLastFocused() {
        return this.lastFocused;
    }

    @Override
    public synchronized @Nullable IChannel getLastPersist() {
        return this.lastPersist;
    }

    @Override
    public synchronized @Nullable IChatter getLastPartner() {
        if (this.lastPartner != null) {
            final IChatter partner = this.instance.getChatterManager().getChatter(this.lastPartner);

            if (partner != null) {
                return partner;
            }

            this.lastPartner = null;
        }

        return null;
    }

    @Override
    public synchronized boolean setLastPartner(@NotNull final IChatter partner) {
        if (partner.getUniqueId().equals(this.lastPartner)) {
            return false;
        }

        this.lastPartner = partner.getUniqueId();
        return true;
    }

    // Channels Collection Methods:
    protected final @NotNull Set<IChannel> _getChannels() {
        if (!this.storage.contains("channels")) {
            this._setChannels(Collections.emptySet());
            this._delayedSave();
        }

        final Set<IChannel> channels = new HashSet<>();

        for (final String name : this.storage.getStringList("channels")) {
            if (name.isEmpty()) {
                continue;
            }

            final IChannel persist = this.instance.getChannelManager().getPersist(name);

            if (persist != null && !persist.isBanned(this.player.getUniqueId())) {
                channels.add(persist);
            }
        }

        return channels;
    }

    @Override
    public synchronized @NotNull Set<IChannel> getChannels() {
        final Set<IChannel> channels = new HashSet<>(this.channels);

        channels.add(this.instance.getChannelManager().getDefault());

        return channels;
    }

    protected final void _setChannels(@NotNull final Set<IChannel> channels) {
        this.storage.set("channels", channels.stream()
                .filter(IChannel::isPersist)
                .map(IChannel::getFullName)
                .collect(Collectors.toList()));
    }

    @Override
    public synchronized boolean joinChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel) || channel.isBanned(this.player.getUniqueId())) {
            return false;
        }

        this.channels.add(channel);

        if (!channel.isMember(this)) {
            channel.addMember(this);
        }

        if (channel.isPersist()) {
            this._setChannels(this.channels);
            this._delayedSave();
        }

        return true;
    }

    @Override
    public synchronized boolean leaveChannel(@NotNull final IChannel channel) {
        if (!this.channels.contains(channel) || channel.isDefault()) {
            return false;
        }

        this.channels.remove(channel);

        if (channel.isMember(this)) {
            channel.removeMember(this);
        }

        // Check if the channel was the last focused channel. When true remove it as last focused channel.
        if (channel.equals(this.lastFocused)) {
            this.lastFocused = null;
        }

        // Check if the channel was the last persist channel. When true remove it as last persist channel.
        if (channel.equals(this.lastPersist)) {
            this.lastPersist = null;
        }

        // Check if the channel is the currently focused channel. When true change focus to last focusable channel.
        if (channel.equals(this.focused)) {
            if (this.setFocus(this.lastFocused != null ? this.lastFocused : this.lastPersist)) {
                this.instance.runSyncTask(() -> this.sendMessage(Messages.tl("focusChannel", this.focused.getColoredName())));
            }
        }

        if (channel.isPersist()) {
            this._setChannels(this.channels);
            this._delayedSave();
        }

        return true;
    }

    @Override
    public synchronized boolean hasChannel(@NotNull final IChannel channel) {
        return this.channels.contains(channel);
    }

    protected final @NotNull Set<UUID> _getIgnores() {
        if (!this.storage.contains("ignores")) {
            this._setIgnores(Collections.emptySet());
            this._delayedSave();
        }

        return new HashSet<>(this.storage.getUniqueIdList("ignores"));
    }

    @Override
    public synchronized @NotNull Set<UUID> getIgnores() {
        return new HashSet<>(this.ignores);
    }

    @Override
    public synchronized boolean setIgnores(@NotNull final Set<UUID> ignores) {
        if (ignores.equals(this.ignores)) {
            return false;
        }

        this.ignores = new HashSet<>(ignores);
        this._setIgnores(ignores);
        this._delayedSave();
        return true;
    }

    protected final void _setIgnores(@NotNull final Set<UUID> ignores) {
        this.storage.set("ignores", ignores.stream().map(UUID::toString).collect(Collectors.toList()));
    }

    @Override
    public synchronized boolean addIgnore(@NotNull final UUID uniqueId) {
        if (this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.add(uniqueId);
        this._setIgnores(this.ignores);
        this._delayedSave();
        return true;
    }

    @Override
    public synchronized boolean removeIgnore(@NotNull final UUID uniqueId) {
        if (!this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.remove(uniqueId);
        this._setIgnores(this.ignores);
        this._delayedSave();
        return true;
    }

    @Override
    public synchronized boolean isIgnore(@NotNull final UUID uniqueId) {
        return this.ignores.contains(uniqueId);
    }

    @Override
    public synchronized boolean isIgnore() {
        return !this.ignores.isEmpty();
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name=");
        builder.append(this.getName());
        builder.append(";uniqueId=");
        builder.append(this.getUniqueId().toString());
        builder.append(";muted=");
        builder.append(this.isMuted());
        builder.append(";focus=");
        builder.append(this.getFocus().getFullName());
        builder.append(";channels=");
        builder.append(this.getChannels().stream()
                .map(IChannel::getFullName)
                .collect(Collectors.joining(",")));

        if (this.isIgnore()) {
            builder.append(";ignores=");
            builder.append(this.getIgnores().stream().map(UUID::toString).collect(Collectors.joining(",")));
        }

        return builder.append("}").toString();
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (object instanceof IChatter) {
            final IChatter other = (IChatter) object;

            return this.getPlayer().equals(other.getPlayer());
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 37;
        int result = 4;

        result = prime * result + this.getPlayer().hashCode();

        return result;
    }

    @Override
    public final int compareTo(@NotNull final IChatter other) {
        return this.getPlayer().getName().compareTo(other.getPlayer().getName());
    }

    // IFilterable Implementation:
    @Override
    public boolean isInRange(@NotNull final IChatter chatter, final int distance) {
        if (!this.isInWorld(chatter)) {
            return false;
        }

        return this.player.getLocation().distanceSquared(chatter.getPlayer().getLocation()) <= (distance * distance);
    }

    @Override
    public boolean isInWorld(@NotNull final IChatter chatter) {
        return this.player.getWorld().equals(chatter.getPlayer().getWorld());
    }

    // ICommandSource Implementation:
    @Override
    public void sendMessage(@NotNull final String message) {
        if (message.isEmpty() || !this.player.isOnline()) {
            return;
        }

        this.player.sendMessage(message);
    }

    @Override
    public boolean hasPermission(@NotNull final String node) {
        return this.player.hasPermission(node);
    }

    // IPermissible Implementation:
    @Override
    public boolean canBan(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be banned.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to being exempt from channel bans.
        if (chatter.hasPermission(Permission.BAN.formChildren("exempt"))) {
            return false;
        }

        return this.canModerate(channel);
    }

    @Override
    public boolean canCreate(@NotNull final ChannelType type) {
        if (type == ChannelType.CONVERSATION) {
            return false;
        }

        return this.player.hasPermission(Permission.CREATE.formChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.DELETE.formChildren("own"))) {
                return true;
            }
        }

        return this.player.hasPermission(Permission.DELETE.formChildren("type", channel.getType().getIdentifier()));
    }

    @Override
    public boolean canFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.FOCUS.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.FOCUS.formChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canIgnore(@NotNull final IChatter chatter) {
        return !chatter.hasPermission(Permission.IGNORE.formChildren("exempt"));
    }

    @Override
    public boolean canJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.JOIN.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.JOIN.formChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canKick(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be kicked.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to being exempt from channel kicks.
        if (chatter.hasPermission(Permission.KICK.formChildren("exempt"))) {
            return false;
        }

        return this.canModerate(channel);
    }

    @Override
    public boolean canLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.LEAVE.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.LEAVE.formChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canList(@NotNull final ChannelType type) {
        return this.player.hasPermission(Permission.LIST.formChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canList(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isDefault() || channel.isMember(this) || channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        return this.canJoin(channel);
    }

    @Override
    public boolean canMessage(@NotNull final IChatter chatter) {
        if (chatter.hasPermission(Permission.MSG.formChildren("exempt"))) {
            return this.player.hasPermission(Permission.MSG.formChildren("exempt", "bypass"));
        }

        return true;
    }

    @Override
    public boolean canModerate(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODERATE.formChildren("own"))) {
                return true;
            }
        }

        if (channel.isModerator(this.player.getUniqueId())) {
            return true;
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODERATE.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.MODERATE.formChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.MODERATE.formChildren("standard", "all"));
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODIFY.formChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODIFY.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.MODIFY.formChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.MODIFY.formChildren("standard", "all"));
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODIFY.formChildren("own"))) {
                return type != ModifyType.OWNER;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODIFY.formChildren("persist", channel.getFullName()))
                    || this.player.hasPermission(Permission.MODIFY.formChildren("persist", "all"))) {
                return this.player.hasPermission(Permission.MODIFY.formChildren("type", type.getIdentifier()));
            }

            return false;
        }

        if (this.player.hasPermission(Permission.MODIFY.formChildren("standard", "all"))) {
            return this.player.hasPermission(Permission.MODIFY.formChildren("type", type.getIdentifier()));
        }

        return false;
    }

    @Override
    public boolean canMute(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be muted.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to being exempt from channel mutes.
        if (chatter.hasPermission(Permission.MUTE.formChildren("exempt"))) {
            return false;
        }

        return this.canModerate(channel);
    }

    @Override
    public boolean canReload(@NotNull final StorageType type) {
        return this.player.hasPermission(Permission.RELOAD.formChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canSave(@NotNull final StorageType type) {
        return this.player.hasPermission(Permission.SAVE.formChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canSee(@NotNull final IChatter chatter) {
        return this.player.canSee(chatter.getPlayer());
    }

    @Override
    public boolean canSpeak(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.SPEAK.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.SPEAK.formChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canView(@NotNull final IChannel channel, @NotNull final InfoType type) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        return this.player.hasPermission(Permission.VIEW.formChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canViewInfo(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.VIEW_INFO.formChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.VIEW_INFO.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.VIEW_INFO.formChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.VIEW_INFO.formChildren("standard", "all"));
    }

    @Override
    public boolean canViewWho(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.VIEW_WHO.formChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.VIEW_WHO.formChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.VIEW_WHO.formChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.VIEW_WHO.formChildren("standard", "all"));
    }

    // IForcible Implementation:
    @Override
    public boolean forcedFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.formChildren("focus", channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.formChildren("join", channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.formChildren("leave", channel.getFullName()));
        }

        return false;
    }
}
