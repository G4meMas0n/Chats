package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.BasicPluginCommand;
import de.g4memas0n.chats.command.chatter.ChatterCommand;
import de.g4memas0n.chats.command.delegate.DelegateCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The help command that allows to show a list of available commands or the help of a command.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 8th, 2020
 * changed: June 22th, 2020
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
                           @NotNull final ICommandInput input) {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMinArgs()) {
                sender.sendMessage(Messages.tl("helpHeader", Messages.tl("commands")));

                for (final BasicCommand command : this.getRegistered()) {
                    if (command instanceof BasicPluginCommand) {
                        if (command instanceof ChatterCommand && !(sender instanceof IChatter)) {
                            return false;
                        }

                        if (sender.hasPermission(command.getPermission())) {
                            sender.sendMessage(Messages.tl("helpCommand", command.getName(), command.getDescription()));
                        }
                    }
                }

                sender.sendMessage(Messages.tl("helpFooter", this.getUsage().replaceAll("\\[(.*?)]", "$1")));
                return true;
            }

            final BasicCommand command = this.getRegistered(input.get(COMMAND));

            if (command == null) {
                sender.sendMessage(Messages.tlErr("commandNotFound", input.get(COMMAND)));
                return true;
            }

            if (sender.hasPermission(command.getPermission())) {
                sender.sendMessage(Messages.tl("helpHeader", command.getName()));
                sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));

                if (!command.getAliases().isEmpty()) {
                    sender.sendMessage(Messages.tlJoin("helpAliases", command.getAliases()));
                }

                if (command instanceof DelegateCommand) {
                    final List<String> commands = new ArrayList<>();

                    for (final BasicCommand delegate : ((DelegateCommand) command).getCommands()) {
                        if (delegate instanceof ChatterCommand && !(sender instanceof IChatter)) {
                            continue;
                        }

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
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == COMMAND + 1) {
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand command : this.getRegistered()) {
                if (command instanceof ChatterCommand && !(sender instanceof IChatter)) {
                    continue;
                }

                if (sender.hasPermission(command.getPermission())) {
                    if (StringUtil.startsWithIgnoreCase(command.getName(), input.get(COMMAND))) {
                        completion.add(command.getName());
                    }

                    for (final String alias : command.getAliases()) {
                        if (StringUtil.startsWithIgnoreCase(alias, input.get(COMMAND))) {
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
