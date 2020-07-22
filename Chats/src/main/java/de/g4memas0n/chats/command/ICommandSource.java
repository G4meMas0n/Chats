package de.g4memas0n.chats.command;

import de.g4memas0n.chats.messaging.IMessageRecipient;
import de.g4memas0n.chats.permission.IPermissible;

/**
 * CommandSource Interface that represents the source of a command, such a chatter, the console or a command block.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface ICommandSource extends IMessageRecipient, IPermissible { }
