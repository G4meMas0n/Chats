package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
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
 * Abstract modifying command representation for commands modifying channels.
 *
 * @author G4meMas0n
 * @since 0.2.4-SNAPSHOT
 *
 * created: June 20th, 2020
 * changed: June 20th, 2020
 */
public abstract class ModifyingCommand extends BasicCommand {

    private static final int CHANNEL = 0;
    private static final int COMMAND = 1;
    private static final int ARGUMENTS = 2;

    private final Map<String, ModifyingSubCommand> commands;

    protected ModifyingCommand(@NotNull final String name,
                               final int initialCapacity,
                               final int loadFactor) {
        super(name, 2, -1);

        this.commands = new HashMap<>(initialCapacity, loadFactor);
    }

    public final @NotNull Set<ModifyingSubCommand> getCommands() {
        return new HashSet<>(this.commands.values());
    }

    public final @Nullable ModifyingSubCommand getCommand(@NotNull final String name) {
        return this.commands.get(name.toLowerCase());
    }

    protected final void addCommand(@NotNull final ModifyingSubCommand command) {
        if (this.commands.containsKey(command.getName())) {
            return;
        }

        this.commands.put(command.getName(), command);
    }

    @SuppressWarnings("unused")
    protected final void removeCommand(@NotNull final ModifyingSubCommand command) {
        if (!this.commands.containsKey(command.getName())) {
            return;
        }

        this.commands.remove(command.getName(), command);
    }

    @Override
    public final boolean execute(@NotNull final ICommandSource sender,
                                 @NotNull final String alias,
                                 @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canModify(channel)) {
                final ModifyingSubCommand command = this.getCommand(arguments[COMMAND]);

                if (command != null) {
                    final String[] converted = arguments.length == ARGUMENTS
                            ? new String[0] : Arrays.copyOfRange(arguments, ARGUMENTS, arguments.length);

                    if (!command.execute(sender, channel, converted)) {
                        sender.sendMessage(Messages.tl("helpHeader", String.format("%s %s", this.getName(), command.getName())));
                        sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                        sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));
                    }

                    return true;
                }

                return false;
            }

            sender.sendMessage(Messages.tl("modifyDenied", channel.getFullName()));
            return true;
        }

        return false;
    }

    @Override
    public final @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                   @NotNull final String alias,
                                                   @NotNull final String[] arguments) {
        if (arguments.length == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModify(channel)) {
                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), arguments[CHANNEL])) {
                        completion.add(channel.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (arguments.length == COMMAND + 1) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyingSubCommand command : this.commands.values()) {
                    if (StringUtil.startsWithIgnoreCase(command.getName(), arguments[COMMAND])) {
                        completion.add(command.getName());
                    }
                }

                Collections.sort(completion);

                return completion;
            }
        }

        if (arguments.length > ARGUMENTS) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final ModifyingSubCommand command = this.getCommand(arguments[COMMAND]);

                if (command != null) {
                    final String[] converted = Arrays.copyOfRange(arguments, ARGUMENTS, arguments.length);

                    return command.tabComplete(sender, channel, converted);
                }
            }
        }

        return Collections.emptyList();
    }
}
