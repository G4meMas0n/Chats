package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Placeholder;
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
 * changed: February 3rd, 2020
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
    private final IChatFormatter formatter;
    private final IChatPerformer performer;
    private String announceFormat;
    private String broadcastFormat;
    private String chatFormat;
    private boolean useCustomFormat;

    public StandardChannel(@NotNull final IChatFormatter formatter,
                           @NotNull final IChatPerformer performer,
                           @NotNull final String fullName) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Name can not be empty");
        }

        this.formatter = formatter;
        this.performer = performer;

        this.fullName = fullName;
        this.chatters = new HashSet<>();

        this.reset();
    }

    @Override
    public void delete() {

    }

    @Override
    public void reload() {

    }

    @Override
    public void reset() {
        this.setShortName(null);
        this.setChatColor(null);
        this.setPassword(null);
        this.setCrossWorld(true);
        this.setDistance(-1);
        this.setAnnounceFormat(null);
        this.setBroadcastFormat(null);
        this.setChatFormat(null);
        this.setUseCustomFormat(false);
    }

    @Override
    public void save() {

    }

    // Channel Properties Methods:
    @Override
    public @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public @NotNull String getShortName() {
        return this.shortName != null && !this.shortName.isEmpty() ? this.shortName : this.fullName;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        if (shortName == null) {
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
        return this.chatColor != null ? this.chatColor : this.formatter.getChatColor();
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) {
        if (this.chatColor == color) {
            return false;
        }

        this.chatColor = color;
        return true;
    }

    @Override
    public boolean hasPassword() {
        return this.password != null && !this.password.isEmpty();
    }

    @Override
    public @Nullable String getPassword() {
        return this.password;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        if (password == null) {
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
        builder.append(";conversion=");
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
        final int prime = 59;
        int result = 5;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getTpe() {
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
    public @NotNull String getAnnounceFormat() {
        if (this.announceFormat == null || this.announceFormat.isEmpty()) {
            return this.formatter.getAnnounceFormat();
        }

        return this.isUseCustomFormat() ? this.announceFormat : this.formatter.getAnnounceFormat();
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format != null && !format.isEmpty() && !format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Invalid format! Format must include the message placeholder");
        }

        if (this.announceFormat.equals(format)) {
            return false;
        }

        this.announceFormat = format;
        return true;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        if (this.broadcastFormat == null || this.broadcastFormat.isEmpty()) {
            return this.formatter.getBroadcastFormat();
        }

        return this.isUseCustomFormat() ? this.broadcastFormat : this.formatter.getBroadcastFormat();
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format != null && !format.isEmpty() && !format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Invalid format! Format must include the message placeholder");
        }

        if (this.broadcastFormat.equals(format)) {
            return false;
        }

        this.broadcastFormat = format;
        return true;
    }

    @Override
    public @NotNull String getChatFormat() {
        if (this.chatFormat == null || this.chatFormat.isEmpty()) {
            return this.formatter.getChatFormat();
        }

        return this.isUseCustomFormat() ? this.chatFormat : this.formatter.getChatFormat();
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (format != null && !format.isEmpty()) {
            if (!format.contains(Placeholder.SENDER.toString()) || !format.contains(Placeholder.MESSAGE.toString())) {
                throw new IllegalArgumentException("Invalid format! Format must include the sender and message placeholder");
            }
        }

        if (this.chatFormat.equals(format)) {
            return false;
        }

        this.chatFormat = format;
        return true;
    }

    @Override
    public boolean isUseCustomFormat() {
        return this.useCustomFormat;
    }

    @Override
    public boolean setUseCustomFormat(final boolean enabled) {
        if (this.useCustomFormat == enabled) {
            return false;
        }

        this.useCustomFormat = enabled;
        return true;
    }

    @Override
    public final @NotNull IChatFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public final @NotNull IChatPerformer getPerformer() {
        return this.performer;
    }
}
