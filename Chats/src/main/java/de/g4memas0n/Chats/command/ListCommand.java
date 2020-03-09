package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.type.ChannelType;
import org.bukkit.command.CommandSender;
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
 * changed: March 3rd, 2020
 */
public final class ListCommand extends BasicCommand {

    private static final String NAME = "list";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = 1;

    private static final int ARG_TYPE = 0;

    private static final String ALL = "all";

    public ListCommand() {
        super(NAME, Permission.CHANNEL_LIST.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);

            if (arguments.length == this.getMaxArgs()) {
                final ChannelType type = ChannelType.getType(arguments[ARG_TYPE]);
                final List<String> channels = new ArrayList<>();

                if (type == null || permissible.canSee(type)) {
                    return false;
                }

                for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                    if (current.getType() != type) {
                        continue;
                    }

                    if (permissible.canSee(current)) {
                        channels.add(current.getColoredName());
                    }
                }

                sender.sendMessage(Messages.tl("listHeader", type.getIdentifier()));
                sender.sendMessage(String.join(Messages.tl("listDelimiter"), channels));
                return true;
            }

            final List<String> channels = new ArrayList<>();

            for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                if (permissible.canSee(current)) {
                    channels.add(current.getColoredName());
                }
            }

            sender.sendMessage(Messages.tl("listHeader", ALL));
            sender.sendMessage(String.join(Messages.tl("listDelimiter"), channels));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMaxArgs()) {
                final List<String> completion = new ArrayList<>();
                final IPermissible permissible = this.getPermissible(sender);

                for (final ChannelType current : ChannelType.values()) {
                    if (InputUtil.containsInput(current.getIdentifier(), arguments[ARG_TYPE])) {
                        if (permissible.canSee(current)) {
                            completion.add(current.getIdentifier());
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
