package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.BasicPluginCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.delegate.DelegateCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlJoin;

/**
 * The help command that allows to show a list of available commands or the help of a command.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class HelpCommand extends BasicCommand {

    private static final int COMMAND = 0;

    public HelpCommand() {
        super("help", 0, 1);

        this.setPermission(Permission.HELP.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMaxArgs()) {
                final BasicCommand command = this.getRegistered(input.get(COMMAND));

                if (command == null || !sender.hasPermission(command.getPermission())) {
                    throw new InvalidArgumentException("commandNotFound", input.get(COMMAND));
                }

                if (command.hide(sender)) {
                    throw new InvalidArgumentException("commandNotAvailable", command.getName());
                }

                sender.sendMessage(tl("helpHeader", String.format("/%s", command.getName())));
                sender.sendMessage(tl("helpDescription", command.getDescription()));
                sender.sendMessage(tl("helpUsage", command.getUsage()));

                if (!command.getAliases().isEmpty()) {
                    sender.sendMessage(tlJoin("helpAliases", command.getAliases()));
                }

                if (command instanceof DelegateCommand) {
                    final List<String> commands = ((DelegateCommand) command).getCommands().stream()
                            .filter(delegate -> sender.hasPermission(delegate.getPermission()) && !delegate.hide(sender))
                            .map(BasicCommand::getName).sorted().collect(Collectors.toList());

                    sender.sendMessage(tlJoin("helpCommands", commands));
                }

                return true;
            }

            final List<String> commands = new ArrayList<>();

            for (final BasicCommand command : this.getRegistered()) {
                if (command instanceof BasicPluginCommand) {
                    if (command.hide(sender)) {
                        continue;
                    }

                    if (sender.hasPermission(command.getPermission())) {
                        commands.add(tl("helpCommand", command.getName(), command.getDescription()));
                    }
                }
            }

            Collections.sort(commands);

            sender.sendMessage(tl("helpHeader", Messages.tl("commands")));
            sender.sendMessage(commands);
            sender.sendMessage(tl("helpFooter", this.getUsage().replaceAll("\\[(.*)]", "$1")));
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
                if (command.hide(sender)) {
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
