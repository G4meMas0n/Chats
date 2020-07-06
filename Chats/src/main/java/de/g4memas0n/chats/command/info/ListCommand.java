package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidArgumentException;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The list command that allows to list available channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
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
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMaxArgs()) {
                final ChannelType type = ChannelType.getType(input.get(TYPE));

                if (type == null) {
                    throw new InvalidArgumentException("invalidType", input.get(TYPE));
                }

                if (sender.canList(type)) {
                    final List<String> channels = new ArrayList<>();

                    for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                        if (channel.getType() != type) {
                            continue;
                        }

                        channels.add(!channel.isConversation() ? channel.getColoredName()
                                : channel.getColor() + channel.getShortName());
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

            Collections.sort(channels);

            sender.sendMessage(Messages.tl("listHeader", Messages.tl("channels")));
            sender.sendMessage(Messages.tlJoin("listChannels", channels));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> help(@NotNull final ICommandSource sender,
                                      @NotNull final ICommandInput input) {
        final List<String> help = super.help(sender, input);

        help.add(Messages.tlJoin("helpTypes", Arrays.stream(ChannelType.values())
                .filter(sender::canList)
                .map(ChannelType::getIdentifier)
                .collect(Collectors.toList())));

        return help;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final ChannelType current : ChannelType.values()) {
                if (sender.canList(current)) {
                    if (StringUtil.startsWithIgnoreCase(current.getIdentifier(), input.get(TYPE))) {
                        completion.add(current.getIdentifier());
                    }
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
