package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Representation of a conversion channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: February 3rd, 2020
 */
public final class ConversationChannel extends StandardChannel {

    private static final String CONVERSATION_NAME = "Conversation";

    public ConversationChannel(@NotNull final IChatFormatter formatter,
                               @NotNull final IChatPerformer performer,
                               @NotNull final IChatter first,
                               @NotNull final IChatter second) throws IllegalArgumentException {
        super(formatter, performer, buildName(first, second));

        super.addChatter(first);
        super.addChatter(second);
    }

    // Channel Properties Methods:
    @Override
    public @NotNull String getShortName() {
        return CONVERSATION_NAME;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        return false;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        return this.getFormatter().getConversationColor();
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) {
        return false;
    }

    @Override
    public boolean hasPassword() {
        return false;
    }

    @Override
    public @Nullable String getPassword() {
        return null;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        return false;
    }

    @Override
    public boolean isCrossWorld() {
        return true;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        return false;
    }

    @Override
    public boolean hasDistance() {
        return false;
    }

    @Override
    public int getDistance() {
        return -1;
    }

    @Override
    public boolean setDistance(final int distance) {
        return false;
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

        final ConversationChannel channel = (ConversationChannel) object;
        return this.getFullName().equals(channel.getFullName());
    }

    @Override
    public int hashCode() {
        final int prime = 67;
        int result = 2;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getTpe() {
        return ChannelType.CONVERSATION;
    }

    @Override
    public boolean isConversation() {
        return true;
    }

    // Channel Collection Methods:
    @Override
    public boolean addChatter(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        return false;
    }

    // Channel Formatter and Performer Methods:
    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean isUseCustomFormat() {
        return false;
    }

    @Override
    public boolean setUseCustomFormat(final boolean enabled) {
        return false;
    }

    // Static Methods:
    public static @NotNull String buildName(@NotNull final IChatter first, @NotNull final IChatter second) {
        if (first.compareTo(second) >= 0) {
            return first.getPlayer().getUniqueId() + "_" + second.getPlayer().getUniqueId();
        } else {
            return second.getPlayer().getUniqueId() + "_" + first.getPlayer().getUniqueId();
        }
    }
}
