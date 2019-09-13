package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.chatters.IChatter;
import org.jetbrains.annotations.NotNull;

/**
 * ChannelFormatter Interface that defines a formatter that handle the message formatting of an channel.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 15th, 2019
 * last change: September 11th, 2019
 */
public interface IChannelFormatter {

    /**
     * Updates the prepared formats with the information of the channel stored in this formatter.
     * @throws IllegalArgumentException Thrown when the channel stored in this formatter returns invalid values.
     */
    void update() throws IllegalArgumentException;

    /**
     * Checks if the announce format is loggable.
     * @return true, when the formats contains the {@link Placeholder#CHANNEL_NAME} or {@link Placeholder#CHANNEL_NICK}
     *               placeholder.
     */
    boolean isAnnounceLoggable();

    /**
     * Formats the given param to the announce format of the channel stored in this formatter.
     * @param message the message that should be formatted.
     * @return the formatted message, or an empty string when the channel stored in this formatter do not support this
     *         feature.
     */
    @NotNull String formatAnnounce(@NotNull final String message);

    /**
     * Checks if the broadcast format is loggable.
     * @return true, when the formats contains the {@link Placeholder#CHANNEL_NAME} or {@link Placeholder#CHANNEL_NICK}
     *               placeholder.
     */
    boolean isBroadcastLoggable();

    /**
     * Formats the given param to the broadcast format of the channel stored in this formatter.
     * @param message the message that should be formatted.
     * @return the formatted message, or an empty string when the channel stored in this formatter do not support this
     *         feature.
     */
    @NotNull String formatBroadcast(@NotNull final String message);

    /**
     * Checks if the channel format is loggable.
     * @return true, when the formats contains the {@link Placeholder#CHANNEL_NAME} or {@link Placeholder#CHANNEL_NICK}
     *               placeholder.
     */
    boolean isChatLoggable();

    /**
     * Formats the given params to the channel format of the channel stored in this formatter.
     * @param sender the sender of the given message.
     * @param message the message from the given sender.
     * @return the formatted message, or an empty string when the channel have defined a extended formatChat method.
     */
    @NotNull String formatChat(@NotNull final IChatter sender,
                               @NotNull final String message);
}
