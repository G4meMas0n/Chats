package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Broadcast Command, extends {@link ModerateCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 7th, 2020
 * changed: June 19th, 2020
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
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canModerate(channel)) {
                final StringBuilder message = new StringBuilder();

                for (int i = MESSAGE; i < arguments.length; i++) {
                    message.append(arguments[i]).append(" ");
                }

                this.getInstance().runSyncTask(() -> channel.performBroadcast(message.toString().trim()));
                return true;
            }

            sender.sendMessage(Messages.tl("moderateDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
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
