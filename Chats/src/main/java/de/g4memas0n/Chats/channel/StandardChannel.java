package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.event.channel.ChannelAnnounceEvent;
import de.g4memas0n.Chats.event.channel.ChannelBroadcastEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChatChannelEvent;
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

/**
 * Representation of a standard channel that is a non persist channel, implements the {@link IChannel} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * changed: March 7th, 2020
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
    private Set<IChatter> chatters;

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
        this.chatters = new HashSet<>();

        this.distance = -1;
        this.crossWorld = true;
        this.customFormat = false;
    }

    // Channel Properties Methods:
    @Override
    public @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public @NotNull String getColoredName() {
        return this.chatColor + this.fullName;
    }

    @Override
    public @NotNull String getShortName() {
        return this.shortName != null ? this.shortName : this.fullName;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) throws IllegalArgumentException {
        if (shortName == null) {
            if (this.shortName == null) {
                return false;
            }

            this.shortName = null;
            return true;
        }

        if (shortName.isEmpty()) {
            throw new IllegalArgumentException("Short name can not be empty");
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
            throw new IllegalArgumentException("Color can not be a format");
        }

        if (this.chatColor == color) {
            return false;
        }

        this.chatColor = color;
        return true;
    }

    @Override
    public boolean hasPassword() {
        return this.password != null;
    }

    @Override
    public @Nullable String getPassword() {
        return this.password;
    }

    @Override
    public boolean setPassword(@Nullable final String password) throws IllegalArgumentException {
        if (password == null) {
            if (this.password == null) {
                return false;
            }

            this.password = null;
            return true;
        }

        if (password.isEmpty()) {
            throw new IllegalArgumentException("Password can not be empty");
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
    public boolean setCrossWorld(final boolean enabled) {
        if (this.crossWorld == enabled) {
            return false;
        }

        this.crossWorld = enabled;
        return true;
    }

    @Override
    public boolean hasDistance() {
        return this.distance > 0;
    }

    @Override
    public int getDistance() {
        return this.hasDistance() ? this.distance : -1;
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
    public int compareTo(@NotNull final IChannel channel) {
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
    public final @NotNull Set<IChatter> getChatters() {
        return new HashSet<>(this.chatters);
    }

    @Override
    public boolean addChatter(@NotNull final IChatter chatter) {
        if (this.chatters.contains(chatter)) {
            return false;
        }

        return this.chatters.add(chatter);
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.contains(chatter)) {
            return false;
        }

        return this.chatters.remove(chatter);
    }

    @Override
    public boolean hasChatter(@NotNull final IChatter chatter) {
        return this.chatters.contains(chatter);
    }

    // Channel Formatter and Performer Methods:
    @Override
    public final @NotNull IFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        if (this.announceFormat == null) {
            return this.formatter.getAnnounceFormat();
        }

        return this.isCustomFormat() ? this.announceFormat : this.formatter.getAnnounceFormat();
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
        if (this.broadcastFormat == null) {
            return this.formatter.getBroadcastFormat();
        }

        return this.isCustomFormat() ? this.broadcastFormat : this.formatter.getBroadcastFormat();
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
        if (this.chatFormat == null) {
            return this.formatter.getChatFormat();
        }

        return this.isCustomFormat() ? this.chatFormat : this.formatter.getChatFormat();
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
        final ChannelAnnounceEvent event = new ChannelAnnounceEvent(this, this.getAnnounceFormat(), message);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.getFormatter().formatAnnounce(this, event.getFormat(), event.getMessage());

        Log.getChatLogger().info(output);

        this.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performBroadcast(@NotNull final String message) {
        final ChannelBroadcastEvent event = new ChannelBroadcastEvent(this, this.getBroadcastFormat(), message);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = this.getFormatter().formatBroadcast(this, event.getFormat(), event.getMessage());

        Log.getChatLogger().info(output);

        this.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.hasChatter(sender)) {
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

            this.getChatters().stream()
                    .filter(chatter -> chatter.isInRange(sender, distance))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        if (!this.isCrossWorld()) {
            this.getChatters().stream()
                    .filter(chatter -> chatter.isInWorld(sender))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        this.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }
}
