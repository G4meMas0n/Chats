package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlErr;

/**
 * The delete command that allows to delete a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
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
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canDelete(channel)) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canDelete(channel)) {
                if (channel.isDefault()) {
                    sender.sendMessage(tlErr("deleteDefault"));
                    return true;
                }

                if (this.getInstance().getChannelManager().removeChannel(channel)) {
                    sender.sendMessage(tl("deleteChannel", channel.getFullName()));
                    return true;
                }

                sender.sendMessage(tlErr("deleteAlready", channel.getFullName()));
                return true;
            }

            sender.sendMessage(tl("deleteDenied", channel.getFullName()));
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
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canDelete(channel)) {
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
