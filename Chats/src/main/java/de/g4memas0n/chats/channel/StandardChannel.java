package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.event.channel.ChannelAnnounceEvent;
import de.g4memas0n.chats.event.channel.ChannelBroadcastEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterBanEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterJoinedEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterKickEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterLeftEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterMuteEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterPardonEvent;
import de.g4memas0n.chats.event.channel.ChannelChatterUnmuteEvent;
import de.g4memas0n.chats.event.chatter.ChatterChatChannelEvent;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.util.logging.Log;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Implementation of a standard channel that is non persist over server restarts.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class StandardChannel implements IChannel {

    public static final String REGEX = "[a-zA-Z0-9]{3,16}";

    private final IChats instance;

    // Channel Properties Variables:
    private final String fullName;
    private String announceFormat;
    private String broadcastFormat;
    private String chatFormat;
    private String shortName;
    private String password;

    private ChatColor color;

    private UUID owner;

    private boolean crossWorld;
    private boolean customFormat;

    private int distance;

    // Channel Collection Variables:
    private final Set<IChatter> members;
    private Set<UUID> moderators;
    private Set<UUID> mutes;
    private Set<UUID> bans;

    public StandardChannel(@NotNull final IChats instance,
                           @NotNull final String fullName) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Full name can not be empty");
        }

        if (!fullName.matches(REGEX) && !this.isConversation()) {
            throw new IllegalArgumentException(String.format("Full name '%s' is not matching naming regex: %s",
                    fullName, REGEX));
        }

        if (fullName.equalsIgnoreCase("all")) {
            throw new IllegalArgumentException(String.format("Full name equals invalid name: %s", "all"));
        }

        this.instance = instance;
        this.fullName = fullName;
        this.color = ChatColor.WHITE;
        this.crossWorld = true;
        this.customFormat = false;
        this.distance = -1;

        this.members = new HashSet<>();
        this.bans = new HashSet<>();
        this.moderators = new HashSet<>();
        this.mutes = new HashSet<>();
    }

    protected final @NotNull IChats getInstance() {
        return this.instance;
    }

    // Channel Properties Methods:
    @Override
    public final @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public final @NotNull String getColoredName() {
        return this.getColor() + this.fullName;
    }

    @Override
    public synchronized @NotNull String getShortName() {
        return this.shortName != null ? this.shortName : this.fullName;
    }

    @Override
    public synchronized boolean setShortName(@Nullable final String shortName) {
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
    public synchronized @NotNull ChatColor getColor() {
        return this.color;
    }

    @Override
    public synchronized boolean setColor(@Nullable final ChatColor color) throws IllegalArgumentException {
        if (color == null) {
            if (this.color == ChatColor.WHITE) {
                return false;
            }

            this.color = ChatColor.WHITE;
            return true;
        }

        if (!color.isColor()) {
            throw new IllegalArgumentException("Color must be a color code: " + color.name());
        }

        if (color == this.color) {
            return false;
        }

        this.color = color;
        return true;
    }

    @Override
    public synchronized boolean hasPassword() {
        return this.password != null;
    }

    @Override
    public synchronized @Nullable String getPassword() {
        return this.password;
    }

    @Override
    public synchronized boolean setPassword(@Nullable final String password) throws IllegalArgumentException {
        if (password == null || password.isEmpty()) {
            if (this.password == null) {
                return false;
            }

            this.password = null;
            return true;
        }

        if (this.isDefault()) {
            throw new IllegalArgumentException("Password is not allowed for default channel: " + this.getFullName());
        }

        if (password.length() < 3) {
            throw new IllegalArgumentException("Password must be at least 3 characters long: " + password);
        }

        if (password.equals(this.password)) {
            return false;
        }

        this.password = password;
        return true;
    }

    @Override
    public synchronized boolean hasDistance() {
        return this.distance > 0;
    }

    @Override
    public synchronized int getDistance() {
        return this.hasDistance() ? this.distance : -1;
    }

    @Override
    public synchronized boolean setDistance(final int distance) {
        if (distance == this.distance) {
            return false;
        }

        this.distance = distance;
        return true;
    }

    @Override
    public synchronized boolean isCrossWorld() {
        return this.crossWorld && !this.hasDistance();
    }

    @Override
    public synchronized boolean setCrossWorld(final boolean crossWorld) {
        if (crossWorld == this.crossWorld) {
            return false;
        }

        this.crossWorld = crossWorld;
        return true;
    }

    @Override
    public synchronized boolean isCustomFormat() {
        return this.customFormat;
    }

    @Override
    public synchronized boolean setCustomFormat(final boolean enabled) {
        if (enabled == this.customFormat) {
            return false;
        }

        this.customFormat = enabled;
        return true;
    }

    @Override
    public synchronized @NotNull String getAnnounceFormat() {
        return this.announceFormat != null ? this.announceFormat : this.instance.getFormatter().getAnnounceFormat();
    }

    @Override
    public synchronized boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null || format.isEmpty()) {
            if (this.announceFormat == null) {
                return false;
            }

            this.announceFormat = null;
            return true;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        if (format.equals(this.announceFormat)) {
            return false;
        }

        this.announceFormat = format;
        return true;
    }

    @Override
    public synchronized @NotNull String getBroadcastFormat() {
        return this.broadcastFormat != null ? this.broadcastFormat : this.instance.getFormatter().getBroadcastFormat();
    }

    @Override
    public synchronized boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null || format.isEmpty()) {
            if (this.broadcastFormat == null) {
                return false;
            }

            this.broadcastFormat = null;
            return true;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        if (format.equals(this.broadcastFormat)) {
            return false;
        }

        this.broadcastFormat = format;
        return true;
    }

    @Override
    public synchronized @NotNull String getChatFormat() {
        return this.chatFormat != null ? this.chatFormat : this.instance.getFormatter().getChatFormat();
    }

    @Override
    public synchronized boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format == null || format.isEmpty()) {
            if (this.chatFormat == null) {
                return false;
            }

            this.chatFormat = null;
            return true;
        }

        if (!format.contains(Placeholder.SENDER.toString()) && !format.contains(Placeholder.SENDER_PLAIN.toString())) {
            throw new IllegalArgumentException("Format is missing {sender} or {sender-plain} placeholder: " + format);
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        if (format.equals(this.chatFormat)) {
            return false;
        }

        this.chatFormat = format;
        return true;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getType() {
        return ChannelType.STANDARD;
    }

    @Override
    public final boolean isConversation() {
        return this.getType() == ChannelType.CONVERSATION;
    }

    @Override
    public final boolean isPersist() {
        return this.getType() == ChannelType.PERSIST;
    }

    @Override
    public final boolean isStandard() {
        return this.getType() == ChannelType.STANDARD;
    }

    @Override
    public final boolean isDefault() {
        if (this.isPersist()) {
            return this.instance.getChannelManager().getDefault() == this;
        }

        return false;
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{full-name=");
        builder.append(this.getFullName());
        builder.append(";short-name=");
        builder.append(this.getShortName());
        builder.append(";color=");
        builder.append(this.getColor().name());

        if (this.hasPassword()) {
            builder.append(";password=");
            builder.append(this.getPassword());
        }

        if (this.hasDistance()) {
            builder.append(";distance=");
            builder.append(this.getDistance());
        }

        builder.append(";cross-world=");
        builder.append(this.isCrossWorld());

        if (this.isCustomFormat()) {
            builder.append(";announce-format=");
            builder.append(this.getAnnounceFormat());
            builder.append(";broadcast-format=");
            builder.append(this.getBroadcastFormat());
            builder.append(";chat-format=");
            builder.append(this.getChatFormat());
        }

        builder.append(";type=");
        builder.append(this.getType().getIdentifier());

        if (this.isDefault()) {
            builder.append(";default=true");
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

        if (object instanceof IChannel) {
            final IChannel other = (IChannel) object;

            return this.getFullName().equals(other.getFullName()) && this.getType() == other.getType();
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 41;
        int result = 5;

        result = prime * result + this.getFullName().hashCode();
        result = prime * result + this.getType().hashCode();

        return result;
    }

    @Override
    public final int compareTo(@NotNull final IChannel other) {
        return this.getFullName().compareTo(other.getFullName());
    }

    // Channel Collection Methods:
    @Override
    public synchronized @NotNull Set<IChatter> getMembers() {
        return new HashSet<>(this.members);
    }

    @Override
    public synchronized boolean setMember(@NotNull final IChatter chatter, final boolean member) {
        if (member) {
            return this.members.add(chatter);
        } else {
            return this.members.remove(chatter);
        }
    }

    @Override
    public synchronized boolean addMember(@NotNull final IChatter member) {
        if (this.members.contains(member) || this.bans.contains(member.getUniqueId())) {
            return false;
        }

        this.performAnnounce(Messages.tl("announceJoin", this.getColor().toString(), member.getDisplayName()));

        this.setMember(member, true);

        if (!member.hasChannel(this)) {
            member.joinChannel(this);
        }

        this.instance.getServer().getPluginManager().callEvent(new ChannelChatterJoinedEvent(this, member));
        return true;
    }

    @Override
    public synchronized boolean removeMember(@NotNull final IChatter member) {
        if (!this.members.contains(member)) {
            return false;
        }

        this.setMember(member, false);

        if (member.hasChannel(this)) {
            member.leaveChannel(this);
        }

        this.performAnnounce(Messages.tl("announceLeave", this.getColor().toString(), member.getDisplayName()));

        this.instance.getServer().getPluginManager().callEvent(new ChannelChatterLeftEvent(this, member));
        return true;
    }

    @Override
    public synchronized boolean banMember(@NotNull final IChatter member) {
        if (!this.members.contains(member) || this.bans.contains(member.getUniqueId())) {
            return false;
        }

        final ChannelChatterBanEvent event = new ChannelChatterBanEvent(this, member);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        this.setBanned(member.getUniqueId(), true);
        this.setMember(member, false);

        member.sendMessage(Messages.tl("gotBanned", this.getColoredName()));

        if (member.hasChannel(this)) {
            member.leaveChannel(this);
        }

        this.performAnnounce(Messages.tl("announceBanned", this.getColor().toString(), member.getDisplayName()));
        return true;
    }

    @Override
    public synchronized boolean pardonMember(@NotNull final IOfflineChatter chatter) {
        if (!this.bans.contains(chatter.getUniqueId())) {
            return false;
        }

        final ChannelChatterPardonEvent event = new ChannelChatterPardonEvent(this, chatter);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        this.setBanned(chatter.getUniqueId(), false);

        if (chatter instanceof IChatter) {
            ((IChatter) chatter).sendMessage(Messages.tl("gotPardoned", this.getColoredName()));
        }

        this.performAnnounce(Messages.tl("announcePardoned", this.getColor().toString(), chatter.getName()));
        return true;
    }

    @Override
    public synchronized boolean kickMember(@NotNull final IChatter member) {
        if (!this.members.contains(member)) {
            return false;
        }

        final ChannelChatterKickEvent event = new ChannelChatterKickEvent(this, member);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        this.setMember(member, false);

        member.sendMessage(Messages.tl("gotKicked", this.getColoredName()));

        if (member.hasChannel(this)) {
            member.leaveChannel(this);
        }

        this.performAnnounce(Messages.tl("announceKicked", this.getColor().toString(), member.getDisplayName()));
        return true;
    }

    @Override
    public synchronized boolean muteMember(@NotNull final IChatter member) {
        if (!this.members.contains(member) || this.mutes.contains(member.getUniqueId())) {
            return false;
        }

        final ChannelChatterMuteEvent event = new ChannelChatterMuteEvent(this, member);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        this.setMuted(member.getUniqueId(), true);

        member.sendMessage(Messages.tl("gotMuted", this.getColoredName()));

        this.performAnnounce(Messages.tl("announceMuted", this.getColor().toString(), member.getDisplayName()));
        return true;
    }

    @Override
    public synchronized boolean unmuteMember(@NotNull final IOfflineChatter member) {
        if (!this.mutes.contains(member.getUniqueId())) {
            return false;
        }

        final ChannelChatterUnmuteEvent event = new ChannelChatterUnmuteEvent(this, member);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return false;
        }

        this.setMuted(member.getUniqueId(), false);

        if (member instanceof IChatter) {
            ((IChatter) member).sendMessage(Messages.tl("gotUnmuted", this.getColoredName()));
        }

        this.performAnnounce(Messages.tl("announceUnmuted", this.getColor().toString(), member.getName()));
        return true;
    }

    @Override
    public synchronized boolean isMember(@NotNull final IChatter chatter) {
        return this.members.contains(chatter);
    }

    @Override
    public synchronized @NotNull Set<UUID> getBans() {
        return new HashSet<>(this.bans);
    }

    @Override
    public synchronized boolean setBans(@NotNull final Set<UUID> bans) {
        if (bans.equals(this.bans)) {
            return false;
        }

        this.bans = new HashSet<>(bans);
        return true;
    }

    @Override
    public synchronized boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        if (banned) {
            return this.bans.add(uniqueId);
        } else {
            return this.bans.remove(uniqueId);
        }
    }

    @Override
    public synchronized boolean isBanned(@NotNull final UUID uniqueId) {
        return this.bans.contains(uniqueId);
    }

    @Override
    public synchronized @NotNull Set<UUID> getModerators() {
        return new HashSet<>(this.moderators);
    }

    @Override
    public synchronized boolean setModerators(@NotNull final Set<UUID> moderators) {
        if (moderators.equals(this.moderators)) {
            return false;
        }

        this.moderators = new HashSet<>(moderators);
        return true;
    }

    @Override
    public synchronized boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator) {
        if (moderator) {
            return this.moderators.add(uniqueId);
        } else {
            return this.moderators.remove(uniqueId);
        }
    }

    @Override
    public synchronized boolean isModerator(@NotNull final UUID uniqueId) {
        return this.moderators.contains(uniqueId);
    }

    @Override
    public synchronized @NotNull Set<UUID> getMutes() {
        return new HashSet<>(this.mutes);
    }

    @Override
    public synchronized boolean setMutes(@NotNull final Set<UUID> mutes) {
        if (mutes.equals(this.mutes)) {
            return false;
        }

        this.mutes = new HashSet<>(mutes);
        return true;
    }

    @Override
    public synchronized boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        if (muted) {
            return this.mutes.add(uniqueId);
        } else {
            return this.mutes.remove(uniqueId);
        }
    }

    @Override
    public synchronized boolean isMuted(@NotNull final UUID uniqueId) {
        return this.mutes.contains(uniqueId);
    }

    @Override
    public synchronized boolean hasOwner() {
        return this.owner != null;
    }

    @Override
    public synchronized @Nullable UUID getOwner() {
        return this.owner;
    }

    @Override
    public synchronized boolean setOwner(@Nullable final UUID uniqueId) {
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
    public synchronized boolean isOwner(@NotNull final UUID uniqueId) {
        return uniqueId.equals(this.owner);
    }

    @Override
    public void performAnnounce(@NotNull final String message) {
        final ChannelAnnounceEvent event = new ChannelAnnounceEvent(this, this.isCustomFormat()
                ? this.getAnnounceFormat() : this.instance.getFormatter().getAnnounceFormat(), message);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.instance.getFormatter().formatAnnounce(this, event.getFormat(),
                event.getMessage());

        Log.getChat().info(String.format("[%s] %s", this.fullName, output));

        this.getMembers().forEach(chatter -> chatter.sendMessage(output));
    }

    @Override
    public void performBroadcast(@NotNull final String message) {
        final ChannelBroadcastEvent event = new ChannelBroadcastEvent(this, this.isCustomFormat()
                ? this.getBroadcastFormat() : this.instance.getFormatter().getBroadcastFormat(), message);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.instance.getFormatter().formatBroadcast(this, event.getFormat(),
                event.getMessage());

        Log.getChat().info(String.format("[%s] %s", this.fullName, output));

        this.getMembers().forEach(chatter -> chatter.sendMessage(output));
    }

    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.isMember(sender) || !sender.getPlayer().isOnline()) {
            return;
        }

        final ChatterChatChannelEvent event = new ChatterChatChannelEvent(sender, this,
                this.isCustomFormat() ? this.getChatFormat() : this.instance.getFormatter().getChatFormat(),
                message, !Bukkit.isPrimaryThread());

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.instance.getFormatter().formatChat(this, event.getFormat(), sender,
                event.getMessage());

        Log.getChat().info(String.format("[%s] %s", this.fullName, output));

        if (this.hasDistance()) {
            final int distance = this.getDistance();

            this.getMembers().stream()
                    .filter(chatter -> chatter.isInRange(sender, distance))
                    .forEach(chatter -> chatter.sendMessage(output));
            return;
        }

        if (!this.isCrossWorld()) {
            this.getMembers().stream()
                    .filter(chatter -> chatter.isInWorld(sender))
                    .forEach(chatter -> chatter.sendMessage(output));
            return;
        }

        this.getMembers().forEach(chatter -> chatter.sendMessage(output));
    }
}
