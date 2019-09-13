package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.chatters.IChatter;
import org.jetbrains.annotations.NotNull;
import java.util.Set;

/**
 * Extended Formatter Interface for the conversion channel type to handle the twitter-style format.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 20th, 2019
 * last change: September 11th, 2019
 */
public interface IConversionFormatter extends IChannelFormatter {

    /**
     * Formats the given params to the channel format of the conversion channel.
     * @param sender the sender of the given message.
     * @param partners the partners that sees this message.
     * @param message the message that should be formatted.
     * @return the formatted message.
     */
    @NotNull String formatChat(@NotNull final IChatter sender,
                               @NotNull final Set<IChatter> partners,
                               @NotNull final String message);

    /**
     * Formats the given params to the twitter-style format of the conversion channel.
     * @param sender the sender of the given message.
     * @param message the message that should be formatted.
     * @return the formatted twitter-style message for the conversion address "from".
     */
    @NotNull String formatFrom(@NotNull final IChatter sender,
                               @NotNull final String message);

    /**
     * Formats the given params to the twitter-style format of the conversion channel.
     * @param partners the sender of the given message.
     * @param message the message that should be formatted.
     * @return the formatted twitter-style message for the conversion address "to".
     */
    @NotNull String formatTo(@NotNull final Set<IChatter> partners,
                             @NotNull final String message);
}
