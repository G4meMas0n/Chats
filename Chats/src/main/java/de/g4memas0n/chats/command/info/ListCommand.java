package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlJoin;

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

        this.setPermission(Permission.LIST.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMaxArgs()) {
                final ChannelType type = ChannelType.getType(input.get(TYPE));

                if (type == null) {
                    throw new InvalidArgumentException("typeNotFound", input.get(TYPE));
                }

                if (sender.canList(type)) {
                    final List<String> channels = this.getInstance().getChannelManager().getChannels().stream()
                            .filter(channel -> channel.getType() == type).sorted()
                            .map(IChannel::getColoredName).collect(Collectors.toList());

                    if (channels.isEmpty()) {
                        sender.sendMessage(tl("listEmpty", tl(type.getIdentifier())));
                        return true;
                    }

                    sender.sendMessage(tl("listHeader", tl(type.getIdentifier())));
                    sender.sendMessage(tlJoin("listChannels", channels));
                    return true;
                }

                sender.sendMessage(tl("listDenied", tl(type.getIdentifier())));
                return true;
            }

            final List<String> channels = this.getInstance().getChannelManager().getChannels().stream()
                    .filter(channel -> sender.canList(channel) || channel.isDefault()).sorted()
                    .map(IChannel::getColoredName).collect(Collectors.toList());

            sender.sendMessage(tl("listHeader", tl("channels")));
            sender.sendMessage(tlJoin("listChannels", channels));
            return true;
        }

        return false;
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
