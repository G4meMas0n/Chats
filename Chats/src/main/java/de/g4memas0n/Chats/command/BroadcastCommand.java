package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Broadcast Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 7th, 2020
 * changed: March 5th, 2020
 */
public final class BroadcastCommand extends BasicCommand {

    private static final String NAME = "broadcast";
    private static final int MIN_ARGS = 2;
    private static final int MAX_ARGS = -1;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_MESSAGE = 1;

    public BroadcastCommand() {
        super(NAME, Permission.CHANNEL_BROADCAST.getName(), MIN_ARGS, MAX_ARGS, Collections.singletonList("bc"));
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[ARG_CHANNEL]));
                return true;
            }

            if (permissible.canBroadcast(channel)) {
                channel.performBroadcast(copyMessage(arguments, ARG_MESSAGE));
                return true;
            }

            sender.sendMessage(Messages.tlErr("broadcastDenied", channel.getFullName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == ARG_CHANNEL + 1) {
                final List<String> completion = new ArrayList<>();
                final IPermissible permissible = this.getPermissible(sender);

                for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                    if (current.isConversation()) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                        if (permissible.canBroadcast(current)) {
                            completion.add(current.getFullName());
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
