package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The List Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: June 20th, 2020
 */
public final class ListCommand extends BasicCommand {

    private static final int TYPE = 0;

    public ListCommand() {
        super("list", 0, 1);

        this.setDescription("Lists all available channels.");
        this.setPermission(Permission.LIST.getNode());
        this.setUsage("/channel list [<type>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMaxArgs()) {
                final ChannelType type = ChannelType.getType(arguments[TYPE]);

                if (type == null) {
                    sender.sendMessage(Messages.tlErr("invalidType"));
                    return true;
                }

                if (sender.canList(type)) {
                    final List<String> channels = new ArrayList<>();

                    if (type == ChannelType.CONVERSATION) {
                        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                            if (!channel.isConversation()) {
                                continue;
                            }

                            channels.add(channel.getColor() + channel.getShortName());
                        }
                    } else {
                        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                            if (channel.getType() != type) {
                                continue;
                            }

                            channels.add(channel.getColoredName());
                        }
                    }

                    if (channels.isEmpty()) {
                        sender.sendMessage(Messages.tl("listEmpty", Messages.tlType(type)));
                        return true;
                    }

                    Collections.sort(channels);

                    sender.sendMessage(Messages.tl("listHeader", Messages.tlType(type)));
                    sender.sendMessage(Messages.tlJoin("listChannels", channels));
                    return true;
                }

                // Check if type is CONVERSATION, to hide this type for users with no permission.
                if (type == ChannelType.CONVERSATION) {
                    sender.sendMessage(Messages.tlErr("invalidType"));
                    return true;
                }

                sender.sendMessage(Messages.tl("listDenied", Messages.tlType(type)));
                return true;
            }

            final List<String> channels = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canList(channel) || channel.isDefault()) {
                    channels.add(channel.getColoredName());
                }
            }

            // Should be always false, but is checked to ensure that the collection is not empty.
            if (channels.isEmpty()) {
                channels.add(this.getInstance().getChannelManager().getDefault().getColoredName());
            }

            sender.sendMessage(Messages.tl("listHeader", Messages.tl("channels")));
            sender.sendMessage(Messages.tlJoin("listChannels", channels));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final ChannelType current : ChannelType.values()) {
                if (sender.canList(current)) {
                    if (StringUtil.startsWithIgnoreCase(current.getIdentifier(), arguments[TYPE])) {
                        completion.add(current.getIdentifier());
                    }
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
