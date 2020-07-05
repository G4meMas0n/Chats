package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
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
 * The broadcast command that allows to send broadcasts to channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 7th, 2020
 * changed: July 4th, 2020
 */
public final class BroadcastCommand extends ModerateCommand {

    private static final int CHANNEL = 0;
    private static final int MESSAGE = 1;

    public BroadcastCommand() {
        super("broadcast", 2, -1);

        this.setAliases(Collections.singletonList("bc"));
        this.setDescription("Broadcasts a message to a channel.");
        this.setPermission(Permission.BROADCAST.getNode());
        this.setUsage("/channel (broadcast|bc) <channel> <message>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canModerate(channel)) {
                this.getInstance().runSyncTask(() -> channel.performBroadcast(input.getMessage(MESSAGE)));
                return true;
            }

            sender.sendMessage(Messages.tl("moderateDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
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
