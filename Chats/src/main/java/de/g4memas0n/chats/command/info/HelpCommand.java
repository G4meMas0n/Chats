package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.BasicPluginCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlErr;

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

        this.setDescription("Shows a list of available commands or the help of a command.");
        this.setPermission(Permission.HELP.getNode());
        this.setUsage("/chats help [<command>]");
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMaxArgs()) {
                final BasicCommand command = this.getRegistered(input.get(COMMAND));

                if (command == null || command.hide(sender) || !sender.hasPermission(command.getPermission())) {
                    sender.sendMessage(tlErr("commandNotFound", input.get(COMMAND)));
                    return true;
                }

                sender.sendMessage(tl("helpHeader", command.getName()));
                sender.sendMessage(tl("helpDescription", command.getDescription()));
                sender.sendMessage(tl("helpUsage", command.getUsage()));
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
