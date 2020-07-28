package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.event.chatter.ChatterFocusChangedEvent;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.util.type.ChannelType;
import de.g4memas0n.chats.util.type.InfoType;
import de.g4memas0n.chats.util.type.ModifyType;
import de.g4memas0n.chats.util.type.StorageType;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.stream.Collectors;

/**
 * Implementation of a standard currently online chatter.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class StandardChatter extends StorageChatter implements IChatter, ICommandSource {

    private final Player player;

    private final Set<IChannel> channels;

    private Set<UUID> ignores;

    private IChannel focused;
    private IChannel lastFocused;
    private IChannel lastPersist;

    private WeakReference<IChatter> lastPartner;

    private boolean muted;
    private boolean socialSpy;

    protected StandardChatter(@NotNull final Chats instance,
                              @NotNull final YamlStorageFile storage,
                              @NotNull final Player player) {
        super(instance, storage);

        this.player = player;

        this.channels = new HashSet<>();
        this.ignores = new HashSet<>();
    }

    @Override
    public synchronized void delete() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(true);
        }

        try {
            this.storage.delete();

            this.instance.getLogger().debug(String.format("Deleted storage file '%s' of chatter: %s",
                    this.storage.getFile().getName(), this.player.getName()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to delete storage file '%s' of chatter '%s': %s",
                    this.storage.getFile().getName(), this.player.getName(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            this.instance.getLogger().debug(String.format("Loaded storage file '%s' of chatter: %s",
                    this.storage.getFile().getName(), this.player.getName()));
        } catch (MissingStorageFileException ex) {
            this.instance.getLogger().warning(String.format("Unable to find storage file '%s' of chatter '%s'. Loading "
                            + "default configuration...", this.storage.getFile().getName(), this.player.getName()));

            this.storage.clear();
            this._delayedSave();
        } catch (IOException | InvalidStorageFileException ex) {
            this.instance.getLogger().log(Level.WARNING, String.format("Unable to load storage file '%s' of chatter '%s'. "
                    + "Loading previous configuration...", this.storage.getFile().getName(), this.player.getName()), ex);
        }

        final UUID uniqueId = this._getUniqueId();

        if (!this.player.getUniqueId().equals(uniqueId)) {
            if (this.storage.getFile().exists()) {
                this.instance.getLogger().warning(String.format("Detected %s unique-id in storage file '%s' of chatter: %s",
                        uniqueId != null ? "invalid" : "missing", this.storage.getFile().getName(), this.player.getName()));
            }

            this._setUniqueId(this.player.getUniqueId());
            this._delayedSave();
        }

        final String name = this._getLastName();

        if (!this.player.getName().equals(name)) {
            if (this.storage.getFile().exists()) {
                this.instance.getLogger().warning(String.format("Detected %s name in storage file '%s' of chatter: %s",
                        name != null ? "old" : "missing", this.storage.getFile().getName(), this.player.getName()));
            }

            this._setLastName(this.player.getName());
            this._delayedSave();
        }

        if (!this.channels.isEmpty()) {
            for (final Iterator<IChannel> iterator = this.channels.iterator(); iterator.hasNext();) {
                final IChannel channel = iterator.next();

                iterator.remove();

                channel.setMember(this, false);
            }
        }

        this.focused = this._getFocus();

        this.channels.addAll(this._getChannels());

        if (this.channels.add(this.focused) || this.channels.add(this.instance.getChannelManager().getDefault())) {
            this._setChannels(this.channels);
            this._delayedSave();
        }

        for (final Iterator<IChannel> iterator = this.channels.iterator(); iterator.hasNext();) {
            final IChannel channel = iterator.next();

            if (channel.isDefault()) {
                continue;
            }

            if (!this.canJoin(channel)) {
                iterator.remove();

                this._setChannels(this.channels);
                this._delayedSave();
            }
        }

        if (!this.channels.contains(this.focused) || !this.canFocus(this.focused)) {
            this.focused = this.instance.getChannelManager().getDefault();

            this._setFocus(this.focused);
            this._delayedSave();
        }

        this.lastFocused = null;
        this.lastPersist = null;
        this.lastPartner = null;

        this.muted = this._getMuted();
        this.socialSpy = this._getSocialSpy();

        if (this.socialSpy && !this.player.hasPermission(Permission.SOCIAL_SPY.getNode())) {
            this.socialSpy = false;

            this._setSocialSpy(false);
            this._delayedSave();
        }

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
            this._setLastName(this.player.getName());
            this._setFocus(this.focused.isPersist() ? this.focused : this.lastPersist);
            this._setChannels(this.channels);
            this._setMuted(this.muted);
            this._setSocialSpy(this.socialSpy);
            this._setIgnores(this.ignores);
        }

        this._setLastPlayed(System.currentTimeMillis());

        try {
            this.storage.save();

            this.instance.getLogger().debug(String.format("Saved storage file '%s' of chatter: %s",
                    this.storage.getFile().getName(), this.player.getName()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to save storage file '%s' of chatter '%s': %s",
                    this.storage.getFile().getName(), this.player.getName(), ex.getMessage()));
        }
    }

    @Override
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

    @Override
    public final @NotNull String getName() {
        return this.player.getName();
    }

    @Override
    public final @NotNull String getDisplayName() {
        return this.player.getDisplayName();
    }

    @Override
    public final @NotNull UUID getUniqueId() {
        return this.player.getUniqueId();
    }

    @Override
    public final long getLastPlayed() {
        return System.currentTimeMillis();
    }

    @Override
    public boolean isMuted() {
        return this.muted;
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

    // Active Channel Methods:
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
        final IChatter partner = this.lastPartner != null ? this.lastPartner.get() : null;

        if (partner == null || !partner.equals(this.instance.getChatterManager().getChatter(partner.getUniqueId()))) {
            this.lastPartner = null;

            return null;
        }

        return partner;
    }

    @Override
    public synchronized boolean setLastPartner(@NotNull final IChatter partner) throws IllegalArgumentException {
        if (partner.getUniqueId().equals(this.player.getUniqueId())) {
            throw new IllegalArgumentException("Partner can not be himself");
        }

        if (partner.equals(this.lastPartner != null ? this.lastPartner.get() : null)) {
            return false;
        }

        this.lastPartner = new WeakReference<>(partner);
        return true;
    }

    // Channels Collection Methods:
    @Override
    public synchronized @NotNull Set<IChannel> getChannels() {
        return new HashSet<>(this.channels);
    }

    @Override
    public boolean joinChannel(@NotNull final IChannel channel) {
        return this.joinChannel(channel, false);
    }

    @Override
    public synchronized boolean joinChannel(@NotNull final IChannel channel, final boolean silent) {
        if (this.channels.contains(channel)) {
            return false;
        }

        this.channels.add(channel);

        if (!channel.isMember(this)) {
            channel.addMember(this, silent);
        }

        if (channel.isPersist()) {
            this._setChannels(this.channels);
            this._delayedSave();
        }

        return true;
    }

    @Override
    public boolean leaveChannel(@NotNull final IChannel channel) {
        return this.leaveChannel(channel, false);
    }

    @Override
    public synchronized boolean leaveChannel(@NotNull final IChannel channel, final boolean silent) {
        if (!this.channels.contains(channel)) {
            return false;
        }

        this.channels.remove(channel);

        if (channel.isMember(this)) {
            channel.removeMember(this, silent);
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
    public final int compareTo(@NotNull final IChatter other) {
        return this.player.getName().compareTo(other.getName());
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (object instanceof StandardChatter) {
            final StandardChatter other = (StandardChatter) object;

            return this.player.equals(other.player);
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 37;
        int result = 4;

        result = prime * result + this.player.hashCode();

        return result;
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name=");
        builder.append(this.player.getName());
        builder.append(";uniqueId=");
        builder.append(this.player.getUniqueId());
        builder.append(";muted=");
        builder.append(this.muted);
        builder.append(";focus=");
        builder.append(this.focused.getFullName());
        builder.append(";channels=");
        builder.append(this.channels.stream().map(IChannel::getFullName).collect(Collectors.joining(",")));

        if (!this.ignores.isEmpty()) {
            builder.append(";ignores=");
            builder.append(this.ignores.stream().map(UUID::toString).collect(Collectors.joining(",")));
        }

        return builder.append("}").toString();
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

    // IMessageRecipient Implementation:
    @Override
    public final void sendMessage(@NotNull final String message) {
        if (message.isEmpty() || !this.player.isOnline()) {
            return;
        }

        this.player.sendMessage(message);
    }

    @Override
    public final void sendMessage(@NotNull final List<String> messages) {
        if (!this.player.isOnline()) {
            return;
        }

        for (final String message : messages) {
            if (message.isEmpty()) {
                continue;
            }

            this.player.sendMessage(message);
        }
    }

    // IPermissible Implementation:
    @Override
    public final boolean hasPermission(@NotNull final String node) {
        return this.player.hasPermission(node);
    }

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
        if (chatter.hasPermission(Permission.BAN.getChildren("exempt"))) {
            return false;
        }

        return this.canModerate(channel);
    }

    @Override
    public boolean canCreate(@NotNull final ChannelType type) {
        if (type == ChannelType.CONVERSATION) {
            return false;
        }

        return this.player.hasPermission(Permission.CREATE.getChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.DELETE.getChildren("own"))) {
                return true;
            }
        }

        return this.player.hasPermission(Permission.DELETE.getChildren("type", channel.getType().getIdentifier()));
    }

    @Override
    public boolean canFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.FOCUS.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.FOCUS.getChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canIgnore(@NotNull final IChatter chatter) {
        return !chatter.hasPermission(Permission.IGNORE.getChildren("exempt"));
    }

    @Override
    public boolean canJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.getFullName().contains(this.player.getUniqueId().toString());
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.JOIN.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.JOIN.getChildren("persist", "all"));
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
        if (chatter.hasPermission(Permission.KICK.getChildren("exempt"))) {
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
            if (this.player.hasPermission(Permission.LEAVE.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.LEAVE.getChildren("persist", "all"));
        }

        return true;
    }

    @Override
    public boolean canList(@NotNull final ChannelType type) {
        return this.player.hasPermission(Permission.LIST.getChildren("type", type.getIdentifier()));
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
        if (chatter.hasPermission(Permission.MSG.getChildren("exempt"))) {
            return this.player.hasPermission(Permission.MSG.getChildren("exempt", "bypass"));
        }

        return true;
    }

    @Override
    public boolean canModerate(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODERATE.getChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODERATE.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.MODERATE.getChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.MODERATE.getChildren("standard", "all"));
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODIFY.getChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODIFY.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.MODIFY.getChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.MODIFY.getChildren("standard", "all"));
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.MODIFY.getChildren("own"))) {
                return type != ModifyType.OWNER;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.MODIFY.getChildren("persist", channel.getFullName()))
                    || this.player.hasPermission(Permission.MODIFY.getChildren("persist", "all"))) {
                return this.player.hasPermission(Permission.MODIFY.getChildren("type", type.getIdentifier()));
            }

            return false;
        }

        if (this.player.hasPermission(Permission.MODIFY.getChildren("standard", "all"))) {
            return this.player.hasPermission(Permission.MODIFY.getChildren("type", type.getIdentifier()));
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
        if (chatter.hasPermission(Permission.MUTE.getChildren("exempt"))) {
            return false;
        }

        return this.canModerate(channel);
    }

    @Override
    public boolean canReload(@NotNull final StorageType type) {
        return this.player.hasPermission(Permission.RELOAD.getChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canSave(@NotNull final StorageType type) {
        return this.player.hasPermission(Permission.SAVE.getChildren("type", type.getIdentifier()));
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
            if (this.player.hasPermission(Permission.SPEAK.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.SPEAK.getChildren("persist", "all"));
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

        return this.player.hasPermission(Permission.VIEW.getChildren("type", type.getIdentifier()));
    }

    @Override
    public boolean canViewInfo(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.VIEW_INFO.getChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.VIEW_INFO.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.VIEW_INFO.getChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.VIEW_INFO.getChildren("standard", "all"));
    }

    @Override
    public boolean canViewWho(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            if (this.player.hasPermission(Permission.VIEW_WHO.getChildren("own"))) {
                return true;
            }
        }

        if (channel.isPersist()) {
            if (this.player.hasPermission(Permission.VIEW_WHO.getChildren("persist", channel.getFullName()))) {
                return true;
            }

            return this.player.hasPermission(Permission.VIEW_WHO.getChildren("persist", "all"));
        }

        return this.player.hasPermission(Permission.VIEW_WHO.getChildren("standard", "all"));
    }

    // IForcible Implementation:
    @Override
    public boolean forcedFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.getChildren("focus", channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.getChildren("join", channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE.getChildren("leave", channel.getFullName()));
        }

        return false;
    }
}
