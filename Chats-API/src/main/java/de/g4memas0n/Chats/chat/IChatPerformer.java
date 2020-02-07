package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;
import java.util.logging.Logger;

/**
 * ChatPerformer Interface that defines a performer representation to perform all chat actions.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: February 1st, 2020
 */
public interface IChatPerformer {

    /**
     * Returns the logger that is used for logging all chatting actions.
     * @return the chat logger.
     */
    @NotNull Logger getChatLogger();

    /**
     * Sets the logger that is used for logging all chatting actions.
     * @param logger the new chat logger.
     * @return true when the logger was changed as result of this call, false otherwise.
     */
    boolean setChatLogger(@NotNull final Logger logger);

    /**
     * Performs the announce action.
     * Checks all conditions to perform this action successfully and then sends all registered chatters of the given
     * channel the given announce message.
     * @param channel the channel that requires to perform this action.
     * @param message the announce message.
     */
    void performAnnounce(@NotNull final IChannel channel,
                         @NotNull final String message);

    /**
     * Performs the broadcast action.
     * Checks all conditions to perform this action successfully and then sends all registered chatters of the given
     * channel the given broadcast message.
     * @param channel the channel that requires to perform this action.
     * @param message the broadcast message.
     */
    void performBroadcast(@NotNull final IChannel channel,
                          @NotNull final String message);

    /**
     * Performs the chat action. This method will run this action synchronized.
     * When the the given channel represents a conversion channel then this method will run the
     * {@link #performConversion(IChatter sender, IChatter partner, String message)} method to perform the conversion.
     * Checks all conditions to perform this action successfully and then sends all registered chatters of the given
     * channel the given chat message from the given sender.
     * @param channel the channel that requires to perform this action.
     * @param sender the sender of the given message.
     * @param message the chat message from the sender.
     */
    void performChat(@NotNull final IChannel channel,
                     @NotNull final IChatter sender,
                     @NotNull final String message);

    /**
     * Performs the conversion action. Sends all chatters that should receive the given message from the sender.
     * @param sender the sender of the given conversion/private message.
     * @param partner the target chatters that should receive the message from the sender.
     * @param message the message from the sender.
     */
    void performConversion(@NotNull final IChatter sender,
                           @NotNull final IChatter partner,
                           @NotNull final String message);
}
