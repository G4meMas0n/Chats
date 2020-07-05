package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The leave command that allows to leave a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 11th, 2020
 * changed: July 3rd, 2020
 */
public final class LeaveCommand extends ChatterCommand {

    private static final int CHANNEL = 0;

    public LeaveCommand() {
        super("leave", 1, 1);

        this.setDescription("Leaves a channel.");
        this.setPermission(Permission.LEAVE.getNode());
        this.setUsage("/leave <channel>");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canLeave(channel)) {
                if (channel.isDefault()) {
                    sender.sendMessage(Messages.tlErr("leaveDefault"));
                    return true;
                }

                if (sender.leaveChannel(channel)) {
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
    public @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : sender.getChannels()) {
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canLeave(channel)) {
                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), input.get(CHANNEL))) {
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
