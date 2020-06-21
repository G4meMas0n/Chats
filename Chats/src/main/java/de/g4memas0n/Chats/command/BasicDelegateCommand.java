package de.g4memas0n.chats.command;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Abstract Delegate Command Representation. Represent commands that only delegate to other registered commands.
 *
 * @author G4meMas0n
 * @since 0.2.1-SNAPSHOT
 *
 * created: May 29th, 2020
 * changed: June 20th, 2020
 */
public abstract class BasicDelegateCommand extends BasicPluginCommand {

    protected static final int DELEGATE = 0;
    protected static final int ARGUMENTS = 1;

    private final Map<String, BasicCommand> commands;

    protected BasicDelegateCommand(@NotNull final String name) {
        super(name, 1, -1);

        this.commands = new HashMap<>();
    }

    protected BasicDelegateCommand(@NotNull final String name,
                              final int initialCapacity,
                              final int loadFactor) {
        super(name, 1 , -1);

        this.commands = new HashMap<>(initialCapacity, loadFactor);
    }

    @Override
    public final boolean register(@NotNull final Chats instance) {
        if (super.register(instance)) {
            this.commands.values().forEach(command -> command.register(instance));
            return true;
        }

        return false;
    }

    @Override
    public final boolean unregister() {
        if (super.unregister()) {
            this.commands.values().forEach(BasicCommand::unregister);
            return true;
        }

        return false;
    }

    public final @NotNull Set<BasicCommand> getCommands() {
        return new HashSet<>(this.commands.values());
    }

    public final @Nullable BasicCommand getCommand(@NotNull final String name) {
        final BasicCommand delegate = this.commands.get(name.toLowerCase());

        if (delegate != null) {
            return delegate;
        }

        for (final BasicCommand command : this.commands.values()) {
            for (final String alias : command.getAliases()) {
                if (alias.equalsIgnoreCase(name)) {
                    return command;
                }
            }
        }

        return null;
    }

    protected final void addCommand(@NotNull final BasicCommand command) {
        if (this.commands.containsKey(command.getName())) {
            return;
        }

        this.commands.put(command.getName(), command);
    }

    @SuppressWarnings("unused")
    protected final void removeCommand(@NotNull final BasicCommand command) {
        if (!this.commands.containsKey(command.getName())) {
            return;
        }

        this.commands.remove(command.getName(), command);

        if (this.getRegistered(command.getName()) != null) {
            command.unregister();
        }
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final BasicCommand command = this.getCommand(arguments[DELEGATE]);

            if (command == null) {
                return false;
            }

            if (sender.hasPermission(command.getPermission())) {
                final String[] converted = arguments.length == ARGUMENTS
                        ? new String[0] : Arrays.copyOfRange(arguments, ARGUMENTS, arguments.length);

                if (!command.execute(sender, alias, converted)) {
                    sender.sendMessage(Messages.tl("helpHeader", command.getName()));
                    sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                    sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));

                    if (!command.getAliases().isEmpty()) {
                        sender.sendMessage(Messages.tlJoin("helpAliases", command.getAliases()));
                    }
                }

                return true;
            }

            sender.sendMessage(Messages.tl("noPermission"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == DELEGATE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand command : this.commands.values()) {
                if (!sender.hasPermission(command.getPermission())) {
                    continue;
                }

                if (StringUtil.startsWithIgnoreCase(command.getName(), arguments[DELEGATE])) {
                    completion.add(command.getName());
                }

                for (final String current : command.getAliases()) {
                    if (StringUtil.startsWithIgnoreCase(current, arguments[DELEGATE])) {
                        completion.add(current);
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (arguments.length > ARGUMENTS) {
            final BasicCommand command = this.getCommand(arguments[DELEGATE]);

            if (command == null) {
                return Collections.emptyList();
            }

            if (sender.hasPermission(command.getPermission())) {
                final String[] converted = Arrays.copyOfRange(arguments, ARGUMENTS, arguments.length);

                return command.tabComplete(sender, alias, converted);
            }
        }

        return Collections.emptyList();
    }
}
