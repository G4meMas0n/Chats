package de.g4memas0n.chats.command;

import org.jetbrains.annotations.NotNull;

/**
 * Abstract moderate command representation for moderating commands.
 *
 * @author G4meMas0n
 * @since 0.2.4-SNAPSHOT
 *
 * created: June 19th, 2020
 * changed: June 19th, 2020
 */
public abstract class ModerateCommand extends BasicCommand {

    protected ModerateCommand(@NotNull final String name,
                              final int minArgs,
                              final int maxArgs) {
        super(name, minArgs, maxArgs);
    }
}
