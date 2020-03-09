package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Leave Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: March 3rd, 2020
 */
public final class LeaveCommand extends BasicPluginCommand {

    private static final String NAME = "leave";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 1;

    private static final int ARG_CHANNEL = 0;

    public LeaveCommand() {
        super(NAME, Permission.CHANNEL_JOIN.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof Player)) {
                return false;
            }

            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[ARG_CHANNEL]));
                return true;
            }

            if (chatter.canLeave(channel)) {
                if (chatter.removeChannel(channel)) {
                    sender.sendMessage(Messages.tl("leaveChannel", channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("leaveAlready", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("leaveDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof Player)) {
                return Collections.emptyList();
            }

            final List<String> completion = new ArrayList<>();
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            for (final IChannel current : chatter.getChannels()) {
                if (current.isConversation()) {
                    continue;
                }

                if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                    if (chatter.canLeave(current)) {
                        completion.add(current.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
