package de.g4memas0n.chats.command.delegate;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.BasicPluginCommand;
import de.g4memas0n.chats.command.chatter.ChatterCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Abstract delegate command representation that represent commands that only delegate to other registered commands.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: May 29th, 2020
 * changed: June 23th, 2020
 */
public abstract class DelegateCommand extends BasicPluginCommand {

    protected static final int DELEGATE = 0;
    protected static final int ARGUMENTS = 1;

    private final Map<String, BasicCommand> commands;

    protected DelegateCommand(@NotNull final String name,
                              final int commands) {
        super(name, 1 , -1);

        this.commands = new HashMap<>(commands + 1, 1);
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

    public final void addCommand(@NotNull final BasicCommand command) {
        if (this.commands.containsKey(command.getName())) {
            return;
        }

        this.commands.put(command.getName(), command);
    }

    @SuppressWarnings("unused")
    public final void removeCommand(@NotNull final BasicCommand command) {
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
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final BasicCommand delegate = this.getCommand(input.get(DELEGATE));

            if (delegate == null) {
                return false;
            }

            if (sender.hasPermission(delegate.getPermission())) {
                if (!delegate.execute(sender, input.getInput(ARGUMENTS))) {
                    sender.sendMessage(Messages.tl("helpHeader", delegate.getName()));
                    sender.sendMessage(Messages.tl("helpDescription", delegate.getDescription()));
                    sender.sendMessage(Messages.tl("helpUsage", delegate.getUsage()));

                    if (!delegate.getAliases().isEmpty()) {
                        sender.sendMessage(Messages.tlJoin("helpAliases", delegate.getAliases()));
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
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == DELEGATE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand delegate : this.commands.values()) {
                if (delegate instanceof ChatterCommand && !(sender instanceof IChatter)) {
                    continue;
                }

                if (sender.hasPermission(delegate.getPermission())) {
                    if (StringUtil.startsWithIgnoreCase(delegate.getName(), input.get(DELEGATE))) {
                        completion.add(delegate.getName());
                    }

                    for (final String alias : delegate.getAliases()) {
                        if (StringUtil.startsWithIgnoreCase(alias, input.get(DELEGATE))) {
                            completion.add(alias);
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (input.getLength() > ARGUMENTS) {
            final BasicCommand delegate = this.getCommand(input.get(DELEGATE));

            if (delegate == null) {
                return Collections.emptyList();
            }

            if (sender.hasPermission(delegate.getPermission())) {
                return delegate.tabComplete(sender, input.getInput(ARGUMENTS));
            }
        }

        return Collections.emptyList();
    }
}
