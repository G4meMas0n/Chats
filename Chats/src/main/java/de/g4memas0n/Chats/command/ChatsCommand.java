package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Chats Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: March 5th, 2020
 * changed: March 10th, 2020
 */
public final class ChatsCommand extends BasicPluginCommand {

    private static final String NAME = "chats";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = -1;

    private static final int ARG_COMMAND = 0;

    public ChatsCommand() {
        super(NAME, Permission.USE.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final BasicCommand command = this.getRegistered(arguments[ARG_COMMAND]);

            if (command == null) {
                return false;
            }

            if (!sender.hasPermission(command.getPermission())) {
                sender.sendMessage(Messages.tl("noPermission"));
                return true;
            }

            if (!command.execute(sender, arguments[ARG_COMMAND], copyArguments(arguments, ARG_COMMAND + 1))) {
                sender.sendMessage(command.getDescription());
                sender.sendMessage(command.getUsage());
            }

            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMinArgs()) {
                final List<String> completion = new ArrayList<>();

                for (final BasicCommand current : this.getRegistered()) {
                    if (InputUtil.containsInput(current.getName(), arguments[ARG_COMMAND])) {
                        if (sender.hasPermission(current.getPermission())) {
                            completion.add(current.getName());
                        }
                    }
                }

                Collections.sort(completion);

                return completion;
            }

            final BasicCommand command = this.getRegistered(arguments[ARG_COMMAND]);

            if (command != null && sender.hasPermission(command.getPermission())) {
                return command.tabComplete(sender, arguments[ARG_COMMAND],
                        copyArguments(arguments, ARG_COMMAND + 1));
            }
        }

        return Collections.emptyList();
    }
}
