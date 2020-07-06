package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicPluginCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * An abstract chatter command representation that allows the execution/tab-completion only for chatters.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class ChatterCommand extends BasicPluginCommand {

    protected ChatterCommand(@NotNull final String name,
                             final int minArgs,
                             final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public final boolean execute(@NotNull final ICommandSource sender,
                                 @NotNull final ICommandInput input) throws InputException {
        if (sender.getChatter() != null) {
            return this.execute(sender.getChatter(), input);
        }

        sender.sendMessage(Messages.tlErr("commandNotAvailable", this.getName()));
        return true;
    }

    /**
     * Executes the command for the given chatter, returning its success.
     *
     * <p>If false is returned, then the help of the command will be sent to the sender.</p>
     *
     * @param sender the chatter who executed the command.
     * @param input the input of the sender, including used alias and passed arguments.
     * @return true if the command execution was valid, false otherwise.
     */
    public abstract boolean execute(@NotNull final IChatter sender,
                                    @NotNull final ICommandInput input) throws InputException;

    @Override
    public final @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                   @NotNull final ICommandInput input) {
        if (sender.getChatter() != null) {
            return this.tabComplete(sender.getChatter(), input);
        }

        return Collections.emptyList();
    }

    /**
     * Requests a list of possible completions for a command argument.
     *
     * @param sender the chatter who tab-completed the command.
     * @param input the input of the sender, including used alias and the passed arguments including the final partial
     *              argument to be completed.
     * @return a list of possible completions for the final arguments.
     */
    public abstract @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                                      @NotNull final ICommandInput input);
}
