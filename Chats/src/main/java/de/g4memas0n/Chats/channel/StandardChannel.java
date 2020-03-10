package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.event.channel.ChannelAnnounceEvent;
import de.g4memas0n.Chats.event.channel.ChannelBroadcastEvent;
import de.g4memas0n.Chats.event.channel.ChannelChatterBanEvent;
import de.g4memas0n.Chats.event.channel.ChannelChatterJoinedEvent;
import de.g4memas0n.Chats.event.channel.ChannelChatterKickEvent;
import de.g4memas0n.Chats.event.channel.ChannelChatterLeftEvent;
import de.g4memas0n.Chats.event.channel.ChannelChatterMuteEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChatChannelEvent;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.logging.Log;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.messaging.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Representation of a standard channel that is a non persist channel, implements the {@link IChannel} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * changed: March 9th, 2020
 */
public class StandardChannel implements IChannel {

    // Channel Properties Variables:
    private final String fullName;
    private ChatColor chatColor;
    private String shortName;
    private String password;
    private boolean crossWorld;
    private int distance;

    // Channel Collection Variables:
    private final Set<IChatter> members;
    private final Set<UUID> moderators;
    private final Set<UUID> banned;
    private final Set<UUID> muted;

    private UUID owner;

    // Channel Formatter and Performer Variables:
    private final IFormatter formatter;
    private String announceFormat;
    private String broadcastFormat;
    private String chatFormat;
    private boolean customFormat;

    public StandardChannel(@NotNull final IFormatter formatter,
                           @NotNull final String fullName) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Full name can not be empty");
        }

        this.formatter = formatter;

        this.fullName = fullName;

        this.members = new HashSet<>();
        this.moderators = new HashSet<>();
        this.banned = new HashSet<>();
        this.muted = new HashSet<>();

        this.distance = -1;
        this.crossWorld = true;
        this.customFormat = false;
    }

    // Channel Properties Methods:
    @Override
    public final @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public final @NotNull String getColoredName() {
        return this.getChatColor() + this.getFullName();
    }

    @Override
    public @NotNull String getShortName() {
        return this.shortName != null ? this.shortName : this.fullName;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        if (shortName == null || shortName.isEmpty()) {
            if (this.shortName == null) {
                return false;
            }

            this.shortName = null;
            return true;
        }

        if (shortName.equals(this.shortName)) {
            return false;
        }

        this.shortName = shortName;
        return true;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        return this.chatColor != null ? this.chatColor : this.formatter.getChannelColor();
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) throws IllegalArgumentException {
        if (color != null && !color.isColor()) {
            throw new IllegalArgumentException("ChatColor must be a color");
        }

        if (this.chatColor == color) {
            return false;
        }

        this.chatColor = color;
        return true;
    }

    @Override
    public @Nullable String getPassword() {
        return this.password;
    }

    @Override
    public boolean hasPassword() {
        return this.password != null;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        if (password == null || password.isEmpty()) {
            if (this.password == null) {
                return false;
            }

            this.password = null;
            return true;
        }

        if (password.equals(this.password)) {
            return false;
        }

        this.password = password;
        return true;
    }

    @Override
    public boolean isCrossWorld() {
        return this.crossWorld && !this.hasDistance();
    }

    @Override
    public boolean setCrossWorld(final boolean crossWorld) {
        if (this.crossWorld == crossWorld) {
            return false;
        }

        this.crossWorld = crossWorld;
        return true;
    }

    @Override
    public int getDistance() {
        return this.hasDistance() ? this.distance : -1;
    }

    @Override
    public boolean hasDistance() {
        return this.distance > 0;
    }

    @Override
    public boolean setDistance(final int distance) {
        if (this.distance == distance) {
            return false;
        }

        this.distance = distance;
        return true;
    }

    @Override
    public final int compareTo(@NotNull final IChannel channel) {
        return this.getFullName().compareTo(channel.getFullName());
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder();

        builder.append(this.getClass().getName());
        builder.append("{full-name=");
        builder.append(this.getFullName());
        builder.append(";short-name=");
        builder.append(this.getShortName());
        builder.append(";chat-color=");
        builder.append(this.getChatColor());

        if (this.hasPassword()) {
            builder.append(";password=");
            builder.append(this.getPassword());
        }

        builder.append(";cross-world=");
        builder.append(this.isCrossWorld());
        builder.append(";distance=");
        builder.append(this.getDistance());
        builder.append(";conversation=");
        builder.append(this.isConversation());
        builder.append(";persist=");
        builder.append(this.isPersist());
        builder.append("}");

        return builder.toString();
    }

    @Override
    public boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final StandardChannel channel = (StandardChannel) object;
        return this.fullName.equals(channel.fullName);
    }

    @Override
    public int hashCode() {
        final int prime = 41;
        int result = 5;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getType() {
        return ChannelType.STANDARD;
    }

    @Override
    public boolean isConversation() {
        return false;
    }

    @Override
    public boolean isPersist() {
        return false;
    }

    // Channel Collection Methods:
    @Override
    public @NotNull Set<IChatter> getMembers() {
        return new HashSet<>(this.members);
    }

    @Override
    public boolean setMember(@NotNull final IChatter chatter, final boolean member) {
        if (member) {
            return this.members.add(chatter);
        } else {
            return this.members.remove(chatter);
        }
    }

    @Override
    public boolean addMember(@NotNull final IChatter chatter) {
        if (this.isMember(chatter)) {
            return false;
        }

        if (this.setMember(chatter, true)) {
            if (!chatter.hasChannel(this)) {
                if (!chatter.joinChannel(this)) {
                    this.setMember(chatter, false);
                    return false;
                }
            }

            this.performAnnounce(Messages.tl("announceJoin", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            Bukkit.getPluginManager().callEvent(new ChannelChatterJoinedEvent(this, chatter));

            return true;
        }

        return false;
    }

    @Override
    public boolean removeMember(@NotNull final IChatter chatter) {
        if (!this.isMember(chatter)) {
            return false;
        }

        if (this.setMember(chatter, false)) {
            if (chatter.hasChannel(this)) {
                if (!chatter.leaveChannel(this)) {
                    this.setMember(chatter, true);
                    return false;
                }
            }

            this.performAnnounce(Messages.tl("announceLeave", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            Bukkit.getPluginManager().callEvent(new ChannelChatterLeftEvent(this, chatter));

            return true;
        }

        return false;
    }

    @Override
    public boolean isMember(@NotNull final IChatter chatter) {
        return this.members.contains(chatter);
    }

    @Override
    public @NotNull Set<UUID> getBanned() {
        return new HashSet<>(this.banned);
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        if (banned) {
            return this.banned.add(uniqueId);
        } else {
            return this.banned.remove(uniqueId);
        }
    }

    @Override
    public boolean banMember(@NotNull final IChatter chatter) {
        if (this.isBanned(chatter)) {
            return false;
        }

        final ChannelChatterBanEvent event = new ChannelChatterBanEvent(this, chatter);

        Bukkit.getPluginManager().callEvent(event);

        if(event.isCancelled()) {
            return false;
        }

        if (this.setBanned(chatter.getPlayer().getUniqueId(), true)) {
            this.setMember(chatter, false);
            this.performAnnounce(Messages.tl("announceBanned", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            return true;
        }

        return false;
    }

    @Override
    public boolean unBanMember(@NotNull final IChatter chatter) {
        if (!this.isBanned(chatter)) {
            return false;
        }

        if (this.setBanned(chatter.getPlayer().getUniqueId(), false)) {
            this.performAnnounce(Messages.tl("announceUnBanned", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            return true;
        }

        return false;
    }

    @Override
    public boolean isBanned(@NotNull final IChatter chatter) {
        return this.banned.contains(chatter.getPlayer().getUniqueId());
    }

    @Override
    public boolean kickMember(@NotNull final IChatter chatter) {
        if (this.isMember(chatter)) {
            return false;
        }

        final ChannelChatterKickEvent event = new ChannelChatterKickEvent(this, chatter);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        if (this.setMember(chatter, false)) {
            this.performAnnounce(Messages.tl("announceKicked", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            return true;
        }

        return false;
    }

    @Override
    public @NotNull Set<UUID> getMuted() {
        return new HashSet<>(this.muted);
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        if (muted) {
            return this.muted.add(uniqueId);
        } else {
            return this.muted.remove(uniqueId);
        }
    }

    @Override
    public boolean muteMember(@NotNull final IChatter chatter) {
        if (this.isMuted(chatter)) {
            return false;
        }

        final ChannelChatterMuteEvent event = new ChannelChatterMuteEvent(this, chatter);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        if (this.setMuted(chatter.getPlayer().getUniqueId(), true)) {
            this.performAnnounce(Messages.tl("announceMuted", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            return true;
        }

        return false;
    }

    @Override
    public boolean unMuteMember(@NotNull final IChatter chatter) {
        if (!this.isMuted(chatter)) {
            return false;
        }

        if (this.setMuted(chatter.getPlayer().getUniqueId(), false)) {
            this.performAnnounce(Messages.tl("announceUnMuted", chatter.getPlayer().getDisplayName(),
                    this.getColoredName()));

            return true;
        }

        return false;
    }

    @Override
    public boolean isMuted(@NotNull final IChatter chatter) {
        return this.muted.contains(chatter.getPlayer().getUniqueId());
    }

    @Override
    public @NotNull Set<UUID> getModerators() {
        return new HashSet<>(this.moderators);
    }

    @Override
    public boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator) {
        if (moderator) {
            return this.moderators.add(uniqueId);
        } else {
            return this.moderators.remove(uniqueId);
        }
    }

    @Override
    public boolean addModerator(@NotNull final IChatter chatter) {
        if (this.isModerator(chatter)) {
            return false;
        }

        return this.setModerator(chatter.getPlayer().getUniqueId(), true);
    }

    @Override
    public boolean removeModerator(@NotNull final IChatter chatter) {
        if (!this.isModerator(chatter)) {
            return false;
        }

        return this.setModerator(chatter.getPlayer().getUniqueId(), false);
    }

    @Override
    public boolean isModerator(@NotNull final IChatter chatter) {
        return this.moderators.contains(chatter.getPlayer().getUniqueId());
    }

    @Override
    public @Nullable UUID getOwner() {
        return this.owner;
    }

    public boolean hasOwner() {
        return this.owner != null;
    }

    @Override
    public boolean setOwner(@Nullable final UUID uniqueId) {
        if (uniqueId == null) {
            if (this.owner == null) {
                return false;
            }

            this.owner = null;
            return true;
        }

        if (uniqueId.equals(this.owner)) {
            return false;
        }

        this.owner = uniqueId;
        return true;
    }

    @Override
    public boolean isOwner(@NotNull final UUID uniqueId) {
        return uniqueId.equals(this.owner);
    }

    // Channel Formatter and Performer Methods:
    @Override
    public final @NotNull IFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        return this.announceFormat != null ? this.announceFormat : this.formatter.getAnnounceFormat();
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null) {
            if (this.announceFormat == null) {
                return false;
            }

            this.announceFormat = null;
            return true;
        }

        if (format.isEmpty()) {
            throw new IllegalArgumentException("Format can not be empty");
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format must include the message placeholder");
        }

        if (this.announceFormat.equals(format)) {
            return false;
        }

        this.announceFormat = format;
        return true;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        return this.broadcastFormat != null ? this.broadcastFormat : this.formatter.getBroadcastFormat();
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null) {
            if (this.broadcastFormat == null) {
                return false;
            }

            this.broadcastFormat = null;
            return true;
        }

        if (format.isEmpty()) {
            throw new IllegalArgumentException("Format can not be empty");
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format must include the message placeholder");
        }

        if (this.broadcastFormat.equals(format)) {
            return false;
        }

        this.broadcastFormat = format;
        return true;
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.chatFormat != null ? this.chatFormat : this.formatter.getChatFormat();
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null) {
            if (this.chatFormat == null) {
                return false;
            }

            this.chatFormat = null;
            return true;
        }

        if (format.isEmpty()) {
            throw new IllegalArgumentException("Format can not be empty");
        }

        if (!format.contains(Placeholder.SENDER.toString()) || !format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format must include the sender and message placeholder");
        }

        if (this.chatFormat.equals(format)) {
            return false;
        }

        this.chatFormat = format;
        return true;
    }

    @Override
    public boolean isCustomFormat() {
        return this.customFormat;
    }

    @Override
    public boolean setCustomFormat(final boolean enabled) {
        if (this.customFormat == enabled) {
            return false;
        }

        this.customFormat = enabled;
        return true;
    }

    @Override
    public void performAnnounce(@NotNull final String message) {
        final ChannelAnnounceEvent event = new ChannelAnnounceEvent(this,
                this.isCustomFormat() ? this.getAnnounceFormat() : this.getFormatter().getAnnounceFormat(), message);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.getFormatter().formatAnnounce(this, event.getFormat(), event.getMessage());

        Log.getChatLogger().info(output);

        this.getMembers().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performBroadcast(@NotNull final String message) {
        final ChannelBroadcastEvent event = new ChannelBroadcastEvent(this,
                this.isCustomFormat() ? this.getBroadcastFormat() : this.getFormatter().getBroadcastFormat(), message);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.getFormatter().formatBroadcast(this, event.getFormat(), event.getMessage());

        Log.getChatLogger().info(output);

        this.getMembers().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.isMember(sender)) {
            return;
        }

        final ChatterChatChannelEvent event = new ChatterChatChannelEvent(sender, this, this.getChatFormat(),
                message, false);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.getFormatter().formatChat(this, event.getFormat(), sender,
                event.getMessage());

        Log.getChatLogger().info(output);

        if (this.hasDistance()) {
            final int distance = this.getDistance();

            this.getMembers().stream()
                    .filter(chatter -> chatter.isInRange(sender, distance))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        if (!this.isCrossWorld()) {
            this.getMembers().stream()
                    .filter(chatter -> chatter.isInWorld(sender))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        this.getMembers().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }
}
