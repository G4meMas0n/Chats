package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.util.logging.Log;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.util.type.ModifyType;
import de.g4memas0n.Chats.event.chatter.ChatterChannelChangedEvent;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.type.ReloadType;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Representation of a standard chatter, implements {@link IChatter}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 6th, 2019
 * changed: March 9th, 2020
 */
public class StandardChatter implements IChatter {

    private final Player player;

    private final IChannelManager manager;
    private final IStorageFile storage;

    private final Set<IChannel> channels;
    private final Set<UUID> ignores;

    private IChannel focused;
    private IChannel lastFocused;
    private IChannel lastPersist;
    private IChatter lastPartner;

    protected StandardChatter(@NotNull final Player player,
                              @NotNull final IChannelManager manager,
                              @NotNull final File file) {
        this.player = player;
        this.manager = manager;
        this.storage = new YamlStorageFile(file);

        this.channels = new HashSet<>();
        this.ignores = new HashSet<>();

        this.load();
    }

    @Override
    public @NotNull IStorageFile getStorageFile() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();

            if (Log.isDebug()) {
                Log.getPluginLogger().info("Deleted chatter file: " + this.storage.getFile().getName()
                        + " for chatter: " + this.player.getName());
            }
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to delete chatter file: " + this.storage.getFile().getName());
        }
    }

    @Override
    public void load() {
        try {
            this.storage.load();
        } catch (FileNotFoundException ex) {
            Log.getPluginLogger().severe("Unable to find chatter file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Saving default configuration for chatter: " + this.player.getName());

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPluginLogger().warning("Unable to load chatter file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Using default configuration for chatter: " + this.player.getName());

            this.storage.clear();
        }

        for (final IChannel current : this.channels) {
            this.channels.remove(current);
            current.setMember(this, false);
        }

        for (final IChannel current : this._getChannels()) {
            this.channels.add(current);
            current.setMember(this, true);
        }

        this.ignores.clear();

        for (final UUID current : this._getIgnores()) {
            if (!current.equals(this.player.getUniqueId())) {
                this.ignores.add(current);
            }
        }

        this.focused = this._getFocus();
        this.lastFocused = null;
        this.lastPersist = null;
        this.lastPartner = null;

        if (Log.isDebug()) {
            Log.getPluginLogger().info("Loaded chatter file: " + this.storage.getFile().getName()
                    + " for chatter: " + this.player.getName());
        }
    }

    @Override
    public void save() {
        try {
            this.storage.save();

            if (Log.isDebug()) {
                Log.getPluginLogger().info("Saved chatter file: " + this.storage.getFile().getName()
                        + " for chatter: " + this.player.getName());
            }
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to save chatter file: " + this.storage.getFile().getName());
        }
    }

    @Override
    public final @NotNull Player getPlayer() {
        return this.player;
    }

    // Active Channel Methods:
    @Override
    public @NotNull IChannel getFocus() {
        return this.focused;
    }

    private @NotNull IChannel _getFocus() {
        final String fullName = this.storage.getString("focus");

        if (fullName == null || fullName.isEmpty()) {
            return this.manager.getDefault();
        }

        final IChannel focused = this.manager.getChannel(fullName);

        if (focused == null || !focused.isPersist()) {
            return this.manager.getDefault();
        }

        this.channels.add(focused);

        return focused;
    }

    @Override
    public boolean setFocus(@NotNull final IChannel channel) {
        if (channel.equals(this.focused)) {
            return false;
        }

        if (!this.hasChannel(channel)) {
            this.joinChannel(channel);
        }

        if (!this.focused.isConversation()) {
            this.lastFocused = this.focused;

            if (this.focused.isPersist()) {
                this.lastPersist = this.focused;
            }
        }

        final Event event = new ChatterChannelChangedEvent(this, this.focused);

        this.focused = channel;

        Bukkit.getServer().getPluginManager().callEvent(event);

        if (channel.isPersist()) {
            this._setFocus(channel);
        }

        return true;
    }

    private void _setFocus(@NotNull final IChannel focus) {
        this.storage.set("focus", focus.getFullName());
    }

    // Last Sources Methods:
    @Override
    public @Nullable IChannel getLastFocused() {
        return this.lastFocused;
    }

    @Override
    public @Nullable IChannel getLastPersist() {
        return this.lastPersist;
    }

    @Override
    public @Nullable IChatter getLastPartner() {
        return this.lastPartner;
    }

    @Override
    public boolean setLastPartners(@NotNull final IChatter partner) {
        if (partner.equals(this.lastPartner)) {
            return false;
        }

        this.lastPartner = partner;
        return true;
    }

    // Channels Collection Methods:
    @Override
    public @NotNull Set<IChannel> getChannels() {
        return new HashSet<>(this.channels);
    }

    private @NotNull Set<IChannel> _getChannels() {
        final Set<IChannel> channels = new HashSet<>();

        channels.add(this.manager.getDefault());

        for (final String current : this.storage.getStringList("channels")) {
            if (current.isEmpty()) {
                continue;
            }

            final IChannel channel = this.manager.getChannel(current);

            if (channel != null && channel.isPersist()) {
                if (channels.contains(channel)) {
                    continue;
                }

                channels.add(channel);
            }
        }

        return channels;
    }

    private void _setChannels(@NotNull final Set<IChannel> channels) {
        this.storage.set("channels", channels.stream().filter(IChannel::isPersist).collect(Collectors.toSet()));
    }

    @Override
    public boolean joinChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel)) {
            return false;
        }

        if (this.channels.add(channel)) {
            if (channel.addMember(this)) {
                if (channel.isPersist()) {
                    this._setChannels(this.channels);
                }

                return true;
            }

            this.channels.remove(channel);
        }

        return false;
    }

    @Override
    public boolean leaveChannel(@NotNull final IChannel channel) {
        if (!this.channels.contains(channel) || channel.equals(this.manager.getDefault())) {
            return false;
        }

        if (channel.equals(this.focused)) {
            if (this.lastFocused != null) {
                this.setFocus(this.lastFocused);
            } else if (this.lastPersist != null) {
                this.setFocus(this.lastPersist);
            } else {
                this.setFocus(this.manager.getDefault());
            }
        }

        if (this.channels.remove(channel)) {
            if (channel.removeMember(this)) {
                if (channel.isPersist()) {
                    this._setChannels(this.channels);
                }

                return true;
            }

            this.channels.add(channel);
        }

        return false;
    }

    @Override
    public boolean hasChannel(@NotNull final IChannel channel) {
        return this.channels.contains(channel);
    }

    @Override
    public @NotNull Set<UUID> getIgnores() {
        return new HashSet<>(this.ignores);
    }

    private @NotNull Set<UUID> _getIgnores() {
        return new HashSet<>(this.storage.getUniqueIdList("ignores"));
    }

    private void _setIgnores(@NotNull final Set<UUID> ignores) {
        this.storage.set("ignores", ignores.stream().map(UUID::toString).collect(Collectors.toSet()));
    }

    @Override
    public boolean addIgnores(@NotNull final UUID uniqueId) {
        if (this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.add(uniqueId);

        this._setIgnores(this.ignores);

        return true;
    }

    @Override
    public boolean removeIgnores(@NotNull final UUID uniqueId) {
        if (!this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.remove(uniqueId);

        this._setIgnores(this.ignores);

        return true;
    }

    @Override
    public boolean isIgnoring(@NotNull final UUID uniqueId) {
        return this.ignores.contains(uniqueId);
    }

    @Override
    public boolean isIgnoring() {
        return !this.ignores.isEmpty();
    }

    @Override
    public final int compareTo(@NotNull final IChatter chatter) {
        return this.getPlayer().getName().compareTo(chatter.getPlayer().getName());
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder();

        builder.append(this.getClass().getName());
        builder.append("{player='");
        builder.append(this.getPlayer().getName());
        builder.append("';uniqueId='");
        builder.append(this.getPlayer().getUniqueId());
        builder.append("';focused='");
        builder.append(this.getFocus().getFullName());
        builder.append("';channels='");
        builder.append(this.getChannels().stream().map(IChannel::getFullName).collect(Collectors.joining("','")));

        if (this.isIgnoring()) {
            builder.append("';ignores='");
            builder.append(this.getIgnores().stream().map(UUID::toString).collect(Collectors.joining("','")));
        }

        builder.append("'}");

        return builder.toString();
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final StandardChatter chatter = (StandardChatter) object;
        return this.player.equals(chatter.player);
    }

    @Override
    public final int hashCode() {
        final int prime = 37;
        int result = 4;

        result = prime * result + this.player.hashCode();

        return result;
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

    // IPermissible Implementation:
    @Override
    public boolean canBroadcast(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (this.canModerate(channel)) {
            return true;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_BROADCAST.formChildren(channel.getFullName()))
                    || this.player.hasPermission(Permission.CHANNEL_BROADCAST.formAll());
        }

        return false;
    }

    @Override
    public boolean canCreate(@NotNull final ChannelType type) {
        return this.player.hasPermission(Permission.CHANNEL_CREATE.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_CREATE.formWildcard());
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        return this.player.hasPermission(Permission.CHANNEL_DELETE.formChildren(channel.getType().getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_DELETE.formWildcard());
    }

    @Override
    public boolean canFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.isMember(this);
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_FOCUS.formChildren(channel.getFullName()))
                    || this.player.hasPermission(Permission.CHANNEL_FOCUS.formAll());
        }

        return true;
    }

    @Override
    public boolean canIgnore(@NotNull final Player player) {
        if (player.hasPermission(Permission.EXCEPT_IGNORE.getName())) {
            return this.player.hasPermission(Permission.EXCEPT_IGNORE.formChildren("bypass"));
        }

        return true;
    }

    @Override
    public boolean canJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_JOIN.formChildren(channel.getFullName()))
                    || this.player.hasPermission(Permission.CHANNEL_JOIN.formAll());
        }

        return true;
    }

    @Override
    public boolean canLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_LEAVE.formChildren(channel.getFullName()))
                    || this.getPlayer().hasPermission(Permission.CHANNEL_LEAVE.formAll());
        }

        return true;
    }

    @Override
    public boolean canList(@NotNull final ChannelType type) {
        return this.player.hasPermission(Permission.CHANNEL_LIST.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_LIST.formWildcard());
    }

    @Override
    public boolean canMessage(@NotNull final Player player) {
        if (player.hasPermission(Permission.EXCEPT_MSG.getName())) {
            return this.player.hasPermission(Permission.EXCEPT_MSG.formChildren("bypass"));
        }

        return true;
    }

    @Override
    public boolean canModerate(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isModerator(this) || channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_MODERATE.formChildren(channel.getFullName()))
                    || this.player.hasPermission(Permission.CHANNEL_MODERATE.formAll());
        }

        return false;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isOwner(this.getPlayer().getUniqueId())) {
            return true;
        }

        return this.player.hasPermission(Permission.CHANNEL_MODIFY.formChildren(channel.getFullName()))
                || this.player.hasPermission(Permission.CHANNEL_MODIFY.formAll());
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
        if (channel.isConversation()) {
            return false;
        }

        if (this.canModify(channel)) {
            return this.player.hasPermission(Permission.CHANNEL_MODIFY.formChildren(type.getIdentifier()))
                    || this.player.hasPermission(Permission.CHANNEL_MODIFY.formWildcard());
        }

        return false;
    }

    @Override
    public boolean canReload(@NotNull final ReloadType type) {
        return this.player.hasPermission(Permission.ADMIN_RELOAD.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.ADMIN_RELOAD.formWildcard());
    }

    @Override
    public boolean canSpeak(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.isMember(this);
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.CHANNEL_SPEAK.formChildren(channel.getFullName()))
                    || this.player.hasPermission(Permission.CHANNEL_SPEAK.formAll());
        }

        return true;
    }

    @Override
    public boolean canViewInfo(@NotNull final IChannel channel) {
        if (channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        return this.player.hasPermission(Permission.CHANNEL_VIEW.formChildren("info"))
                || this.player.hasPermission(Permission.CHANNEL_VIEW.formWildcard());
    }

    @Override
    public boolean canViewWho(@NotNull final IChannel channel) {
        if (channel.isOwner(this.player.getUniqueId())) {
            return true;
        }

        return this.player.hasPermission(Permission.CHANNEL_VIEW.formChildren("who"))
                || this.player.hasPermission(Permission.CHANNEL_VIEW.formWildcard());
    }

    @Override
    public boolean forcedFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE_FOCUS.formChildren(channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE_JOIN.formChildren(channel.getFullName()));
        }

        return false;
    }

    @Override
    public boolean forcedLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        if (channel.isPersist()) {
            return this.player.hasPermission(Permission.FORCE_LEAVE.formChildren(channel.getFullName()));
        }

        return false;
    }
}
