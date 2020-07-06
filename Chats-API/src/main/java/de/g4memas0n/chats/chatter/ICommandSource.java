package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.messaging.IMessageRecipient;
import de.g4memas0n.chats.permission.IPermissible;
import org.jetbrains.annotations.Nullable;

/**
 * CommandSource Interface that represents the source of a command, such a chatter, the console or a command block.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface ICommandSource extends IMessageRecipient, IPermissible {

    /**
     * Returns the chatter representing this command source, when it it a chatter.
     *
     * <p>Can be null when this command source is not represented by a chatter.</p>
     *
     * @return the chatter representing this command source, or null.
     */
    @Nullable IChatter getChatter();

    /**
     * Returns whether this command source is a chatter or not.
     *
     * @return true when this command source is a chatter, false otherwise.
     */
    boolean isChatter();

    /**
     * Returns whether this command source is the console or not.
     *
     * @return true when this command source is the console, false otherwise.
     */
    boolean isConsole();
}
