package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Help Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: June 20th, 2020
 */
public final class HelpCommand extends BasicCommand {

    private static final int COMMAND = 0;

    public HelpCommand() {
        super("help", 0, 1);

        this.setDescription("Shows a list of available commands or the help of a command.");
        this.setPermission(Permission.HELP.getNode());
        this.setUsage("/chats help [<command>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMinArgs()) {
                sender.sendMessage(Messages.tl("helpHeader", Messages.tl("commands")));

                for (final BasicCommand command : this.getRegistered()) {
                    if (command instanceof BasicPluginCommand) {
                        if (sender.hasPermission(command.getPermission())) {
                            sender.sendMessage(Messages.tl("helpCommand", command.getName(), command.getDescription()));
                        }
                    }
                }

                sender.sendMessage(Messages.tl("helpFooter", this.getUsage().replaceAll("\\[(.*?)]", "$1")));
                return true;
            }

            final BasicCommand command = this.getRegistered(arguments[COMMAND]);

            if (command == null) {
                sender.sendMessage(Messages.tlErr("commandNotFound", arguments[COMMAND]));
                return true;
            }

            if (sender.hasPermission(command.getPermission())) {
                sender.sendMessage(Messages.tl("helpHeader", command.getName()));
                sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));

                if (!command.getAliases().isEmpty()) {
                    sender.sendMessage(Messages.tlJoin("helpAliases", command.getAliases()));
                }

                if (command instanceof BasicDelegateCommand) {
                    final List<String> commands = new ArrayList<>();

                    for (final BasicCommand delegate : ((BasicDelegateCommand) command).getCommands()) {
                        if (sender.hasPermission(delegate.getPermission())) {
                            commands.add(delegate.getName());
                        }
                    }

                    sender.sendMessage(Messages.tlJoin("helpCommands", commands));
                }

                return true;
            }

            sender.sendMessage(Messages.tl("helpNoPermission"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String label,
                                             @NotNull final String[] arguments) {
        if (arguments.length == COMMAND + 1) {
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand command : this.getRegistered()) {
                if (sender.hasPermission(command.getPermission())) {
                    if (StringUtil.startsWithIgnoreCase(command.getName(), arguments[COMMAND])) {
                        completion.add(command.getName());
                    }

                    for (final String alias : command.getAliases()) {
                        if (StringUtil.startsWithIgnoreCase(alias, arguments[COMMAND])) {
                            completion.add(alias);
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
