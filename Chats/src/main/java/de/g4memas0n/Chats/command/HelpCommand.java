package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.util.PageHandler;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
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
 * changed: March 3rd, 2020
 */
public final class HelpCommand extends BasicCommand {

    private static final String NAME = "help";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = 1;

    private static final int ARG_PAGE_COMMAND = 0;

    public HelpCommand() {
        super(NAME, Permission.HELP.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMaxArgs() && !InputUtil.isInt(arguments[ARG_PAGE_COMMAND])) {
                final BasicCommand command = this.getRegistered(arguments[ARG_PAGE_COMMAND]);

                if (command == null) {
                    return false;
                }

                if (!sender.hasPermission(command.getPermission())) {
                    sender.sendMessage(Messages.tl("helpNoPermission"));
                    return true;
                }

                sender.sendMessage(Messages.tl("helpHeader", command.getName()));
                sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));

                if (command.hasAliases()) {
                    sender.sendMessage(Messages.tl("helpAliases",
                            String.join(Messages.tl("listDelimiter"), command.getAliases())));
                }

                return true;
            }

            final PageHandler<BasicCommand> pages = new PageHandler<>();
            int pageNumber = 1;

            for (final BasicCommand current : this.getRegistered()) {
                if (sender.hasPermission(current.getPermission())) {
                    pages.addEntry(current);
                }
            }

            if (arguments.length == this.getMaxArgs()) {
                pageNumber = InputUtil.parseInt(arguments[ARG_PAGE_COMMAND]);

                if (pageNumber <= 0 || pageNumber > pages.getSize()) {
                    sender.sendMessage(Messages.tlErr("pageNotExist"));
                    return true;
                }
            }

            sender.sendMessage(Messages.tl("helpHeaderWithPages", Messages.tl("commands"),
                    pageNumber, pages.getSize()));

            for (final BasicCommand current : pages.getPage(pageNumber - 1).getEntries()) {
                sender.sendMessage(Messages.tl("helpCommand", current.getUsage(), current.getDescription()));
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
            if (arguments.length == this.getMaxArgs()) {
                final List<String> completion = new ArrayList<>();

                for (final BasicCommand current : this.getRegistered()) {
                    if (InputUtil.containsInput(current.getName(), arguments[ARG_PAGE_COMMAND])) {
                        if (sender.hasPermission(current.getPermission())) {
                            completion.add(current.getName());
                        }
                    }
                }

                Collections.sort(completion);

                return completion;
            }
        }

        return Collections.emptyList();
    }
}
