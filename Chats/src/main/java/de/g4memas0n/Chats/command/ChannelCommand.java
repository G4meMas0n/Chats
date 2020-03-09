package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Channel Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 10th, 2020
 * changed: March 3rd, 2020
 */
public final class ChannelCommand extends BasicPluginCommand {

    private static final String NAME = "channel";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = -1;

    private static final int ARG_COMMAND = 0;

    private static final String[] commands = {"broadcast", "chat", "create", "delete", "focus", "join", "leave", "list",
            "modify"};

    public ChannelCommand() {
        super(NAME, Permission.CHANNEL_COMMAND.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final BasicCommand command = this.getSubCommand(arguments[ARG_COMMAND]);

            if (command == null) {
                return false;
            }

            if (!sender.hasPermission(command.getPermission())) {
                sender.sendMessage(Messages.tl("noPermission"));
                return true;
            }

            if (!command.execute(sender, alias, copyArguments(arguments, ARG_COMMAND + 1))) {
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

                for (final String current : commands) {
                    if (InputUtil.containsInput(current, arguments[ARG_COMMAND])) {
                        final BasicCommand command = getRegistered(current);

                        if (command != null && sender.hasPermission(command.getPermission())) {
                            completion.add(current);
                        }
                    }
                }

                return completion;
            }

            final BasicCommand command = this.getSubCommand(arguments[ARG_COMMAND]);

            if (command != null && sender.hasPermission(command.getPermission())) {
                return command.tabComplete(sender, alias, copyArguments(arguments, ARG_COMMAND + 1));
            }
        }

        return Collections.emptyList();
    }

    private @Nullable BasicCommand getSubCommand(@NotNull final String command) {
        for (final String current : commands) {
            if (current.equalsIgnoreCase(command)) {
                return getRegistered(current);
            }
        }

        return null;
    }
}
