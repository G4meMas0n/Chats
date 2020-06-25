package de.g4memas0n.chats.chatter;

import org.jetbrains.annotations.NotNull;

/**
 * CommandSource Interface that represents the source of a command, such a chatter, the console or a command block.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: April 4th, 2020
 * changed: April 19th, 2020
 */
public interface ICommandSource extends IPermissible {

    /**
     * Sends the given message to this command source.
     * @param message the message to send.
     */
    void sendMessage(@NotNull final String message);

    /**
     * Checks whether this command source has the given permission node
     * @param node the permission node to check.
     * @return true when this command source has the permission, false otherwise.
     */
    boolean hasPermission(@NotNull final String node);
}
