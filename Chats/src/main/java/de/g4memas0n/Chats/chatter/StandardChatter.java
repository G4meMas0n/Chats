package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.channel.type.ModifyType;
import de.g4memas0n.Chats.event.chatter.ChatterChannelChangedEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChannelJoinedEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChannelLeavedEvent;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.ReloadType;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
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
 * changed: January 17th, 2020
 */
public class StandardChatter implements IChatter {

    private static final String PATH_CHANNELS = "channels";
    private static final String PATH_FOCUS = "focus";
    private static final String PATH_IGNORES = "ignores";

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

        this.reload();
    }

    @Override
    public void delete() {
        this.storage.delete();
    }

    @Override
    public void reload() {
        try {
            this.storage.load();
        } catch (IOException | InvalidStorageFileException ex) {
            //TODO: Log reload failure.
            return;
        }

        this.reset();

        for (String current : this.storage.getStringList(PATH_CHANNELS)) {
            if (current.isEmpty()) {
                continue;
            }

            final IChannel channel = this.manager.getChannel(current);

            if (channel != null && channel.isPersist()) {
                if (this.channels.contains(channel)) {
                    continue;
                }

                channel.addChatter(this);
                this.channels.add(channel);
            }
        }

        if (this.storage.contains(PATH_FOCUS)) {
            final String name = this.storage.getString(PATH_FOCUS);

            if (name != null && !name.isEmpty()) {
                final IChannel focused = this.manager.getChannel(name);

                if (focused != null && focused.isPersist()) {
                    if (!this.channels.contains(focused)) {
                        focused.addChatter(this);
                        this.channels.add(focused);
                    }

                    this.focused = focused;
                }
            }
        }

        this.ignores.addAll(this.storage.getUniqueIdList(PATH_IGNORES));
    }

    @Override
    public void reset() {
        this.channels.clear();
        this.ignores.clear();

        this.focused = this.manager.getDefault();
        this.lastFocused = null;
        this.lastPersist = null;
        this.lastPartner = null;

        this.channels.add(this.focused);
    }

    @Override
    public void save() {
        this.storage.set(PATH_CHANNELS, this.channels.stream().filter(IChannel::isPersist).collect(Collectors.toSet()));
        this.storage.set(PATH_IGNORES, this.ignores.stream().map(UUID::toString).collect(Collectors.toSet()));
        this.storage.set(PATH_FOCUS, this.focused.isPersist() ? this.focused : this.lastPersist);

        try {
            this.storage.save();
        } catch (IOException ex) {
            //TODO: Log save failure.
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

    @Override
    public boolean setFocus(@NotNull final IChannel channel) {
        if (channel.equals(this.focused)) {
            return false;
        }

        if (!this.channels.contains(channel)) {
            this.addChannel(channel);
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

        this.save();
        return true;
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

    @Override
    public boolean addChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel)) {
            return false;
        }

        this.channels.add(channel);

        if (!channel.hasChatter(this)) {
            channel.getPerformer().performAnnounce(channel, ""); //TODO: Add localized 'channel_chatterJoin' message.
            channel.addChatter(this);
        }

        Bukkit.getServer().getPluginManager().callEvent(new ChatterChannelJoinedEvent(this, channel));

        if (channel.isPersist()) {
            this.save();
        }

        return true;
    }

    @Override
    public boolean removeChannel(@NotNull final IChannel channel) {
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

        this.channels.remove(channel);

        if (channel.hasChatter(this)) {
            channel.removeChatter(this);
            channel.getPerformer().performAnnounce(channel, ""); //TODO: Add localized 'channel_chatterLeave' message.
        }

        Bukkit.getServer().getPluginManager().callEvent(new ChatterChannelLeavedEvent(this, channel));

        if (channel.isPersist()) {
            this.save();
        }

        return true;
    }

    @Override
    public boolean hasChannel(@NotNull final IChannel channel) {
        return this.channels.contains(channel);
    }

    @Override
    public @NotNull Set<UUID> getIgnores() {
        return new HashSet<>(this.ignores);
    }

    @Override
    public boolean addIgnores(@NotNull final UUID uniqueId) {
        if (this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.add(uniqueId);
        this.save();
        return true;
    }

    @Override
    public boolean removeIgnores(@NotNull final UUID uniqueId) {
        if (!this.ignores.contains(uniqueId)) {
            return false;
        }

        this.ignores.remove(uniqueId);
        this.save();
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
    public boolean canCreate(@NotNull final ChannelType type) {
        return this.player.hasPermission(Permission.CHANNEL_CREATE.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_CREATE.formAll());
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.CHANNEL_DELETE.formChildren(channel.getTpe().getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_DELETE.formAll());
    }

    @Override
    public boolean canFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.hasChatter(this);
        }

        return this.player.hasPermission(Permission.CHANNEL_FOCUS.formChildren(channel.getFullName()))
                || this.player.hasPermission(Permission.CHANNEL_FOCUS.formAll());
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

        return this.player.hasPermission(Permission.CHANNEL_JOIN.formChildren(channel.getFullName()))
                || this.player.hasPermission(Permission.CHANNEL_JOIN.formWildcard());
    }

    @Override
    public boolean canLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.CHANNEL_LEAVE.formChildren(channel.getFullName()))
                || this.getPlayer().hasPermission(Permission.CHANNEL_LEAVE.formAll());
    }

    @Override
    public boolean canMessage(@NotNull final Player player) {
        if (player.hasPermission(Permission.EXCEPT_MSG.getName())) {
            return this.player.hasPermission(Permission.EXCEPT_MSG.formChildren("bypass"));
        }

        return true;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.CHANNEL_MODIFY.formAll());
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.CHANNEL_MODIFY.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.CHANNEL_MODIFY.formAll());
    }

    @Override
    public boolean canReload(@NotNull final ReloadType type) {
        return this.player.hasPermission(Permission.ADMIN_RELOAD.formChildren(type.getIdentifier()))
                || this.player.hasPermission(Permission.ADMIN_RELOAD.formWildcard());
    }

    @Override
    public boolean canSpeak(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return channel.hasChatter(this);
        }

        return this.player.hasPermission(Permission.CHANNEL_SPEAK.formChildren(channel.getFullName()))
                || this.player.hasPermission(Permission.CHANNEL_SPEAK.formAll());
    }

    @Override
    public boolean forcedFocus(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.FORCE_FOCUS.formChildren(channel.getFullName()));
    }

    @Override
    public boolean forcedJoin(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.FORCE_JOIN.formChildren(channel.getFullName()));
    }

    @Override
    public boolean forcedLeave(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.player.hasPermission(Permission.FORCE_LEAVE.formChildren(channel.getFullName()));
    }
}
