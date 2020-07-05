package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.command.BasicCommand;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract moderate command representation for commands that moderates a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 19th, 2020
 * changed: July 4th, 2020
 */
public abstract class ModerateCommand extends BasicCommand {

    protected static final int TARGET = 0;
    protected static final int CHANNEL = 1;

    protected ModerateCommand(@NotNull final String name,
                              final int minArgs,
                              final int maxArgs) {
        super(name, minArgs, maxArgs);
    }
}
