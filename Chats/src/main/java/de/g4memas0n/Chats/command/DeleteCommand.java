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
 * The Delete Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: March 10th, 2020
 */
public final class DeleteCommand extends BasicCommand {

    private static final String NAME = "delete";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 1;

    private static final int ARG_CHANNEL = 0;

    public DeleteCommand() {
        super(NAME, Permission.CHANNEL_DELETE.getName(), MIN_ARGS, MAX_ARGS);
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

            if (permissible.canDelete(channel)) {
                try {
                    if (this.getInstance().getChannelManager().removeChannel(channel)) {
                        sender.sendMessage(Messages.tl("deleteChannel", channel.getColoredName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("deleteAlready", channel.getColoredName()));
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("deleteDefault"));
                }

                return true;
            }

            sender.sendMessage(Messages.tl("deleteDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final List<String> completion = new ArrayList<>();
            final IPermissible permissible = this.getPermissible(sender);

            for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                if (current.isConversation()) {
                    continue;
                }

                if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                    if (permissible.canDelete(current)) {
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
