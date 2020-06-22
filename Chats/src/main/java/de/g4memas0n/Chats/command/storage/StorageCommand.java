package de.g4memas0n.chats.command.storage;

import de.g4memas0n.chats.command.BasicCommand;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract storage command representation for commands handling storage container and holders.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 22th, 2020
 * changed: June 22th, 2020
 */
public abstract class StorageCommand extends BasicCommand {

    protected StorageCommand(@NotNull final String name,
                             final int minArgs,
                             final int maxArgs) {
        super(name, minArgs, maxArgs);
    }
}
