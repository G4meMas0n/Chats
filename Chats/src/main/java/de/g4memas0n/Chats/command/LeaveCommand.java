package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Leave Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: June 19th, 2020
 */
public final class LeaveCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public LeaveCommand() {
        super("leave", 1, 1);

        this.setDescription("Leaves a channel.");
        this.setPermission(Permission.LEAVE.getNode());
        this.setUsage("/leave <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter chatter = (IChatter) sender;
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canLeave(channel)) {
                if (channel.isDefault()) {
                    sender.sendMessage(Messages.tlErr("leaveDefault"));
                    return true;
                }

                if (chatter.leaveChannel(channel)) {
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
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == CHANNEL + 1) {
            if (!(sender instanceof IChatter)) {
                return Collections.emptyList();
            }

            final IChatter chatter = (IChatter) sender;
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : chatter.getChannels()) {
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canLeave(channel)) {
                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), arguments[CHANNEL])) {
                        completion.add(channel.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
