package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The chat command that allows to send messages in different channels as the focused channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChatCommand extends ChatterCommand {

    private static final int CHANNEL = 0;
    private static final int MESSAGE = 1;

    public ChatCommand() {
        super("chat", 2, -1);

        this.setDescription("Sends a message in a channel without changing the focused channel.");
        this.setPermission(Permission.SPEAK.getNode());
        this.setUsage("/chat <channel> <message>");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (!sender.hasChannel(channel)) {
                sender.sendMessage(tl("leaveAlready", channel.getColoredName()));
                return true;
            }

            if (sender.canSpeak(channel)) {
                if (channel.isMuted(sender.getUniqueId()) && !sender.hasPermission(Permission.MUTE.getChildren("bypass"))) {
                    sender.sendMessage(tl("mutedMember", channel.getColoredName()));
                    return true;
                }

                this.getInstance().runSyncTask(() -> channel.performChat(sender, input.getMessage(MESSAGE)));
                return true;
            }

            sender.sendMessage(tl("chatDenied", channel.getColoredName()));
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
                if (channel.isConversation() || channel.equals(sender.getFocus())
                        || channel.isMuted(sender.getUniqueId())) {
                    continue;
                }

                if (sender.canSpeak(channel)) {
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
