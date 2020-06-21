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
 * The Delete Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: June 20th, 2020
 */
public final class DeleteCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public DeleteCommand() {
        super("delete", 1, 1);

        this.setDescription("Deletes a channel.");
        this.setPermission(Permission.DELETE.getNode());
        this.setUsage("/channel delete <channel>");
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

            if (sender.canDelete(channel)) {
                if (channel.isDefault()) {
                    sender.sendMessage(Messages.tlErr("deleteDefault"));
                    return true;
                }

                if (this.getInstance().getChannelManager().removeChannel(channel)) {
                    sender.sendMessage(Messages.tl("deleteChannel", channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(Messages.tlErr("deleteAlready", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("deleteDenied", channel.getColoredName()));
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
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canDelete(channel)) {
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
