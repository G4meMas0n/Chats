package de.g4memas0n.chats.messaging;

import org.jetbrains.annotations.NotNull;
import java.util.List;

/**
 * Message Recipient Interface that represents a recipient who can receive messages.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IMessageRecipient {

    /**
     * Sends the given message to this message recipient.
     *
     * @param message the message to send.
     */
    void sendMessage(@NotNull final String message);

    /**
     * Sends the given messages to this message recipient.
     *
     * @param messages the messages to send.
     */
    void sendMessage(@NotNull final List<String> messages);
}
