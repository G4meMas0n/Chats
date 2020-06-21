package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Create Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: June 20th, 2020
 */
public final class CreateCommand extends BasicCommand {

    private static final int NAME = 0;
    private static final int TYPE = 1;

    public CreateCommand() {
        super("create", 1, 2);

        this.setDescription("Creates a new channel.");
        this.setPermission(Permission.CREATE.getNode());
        this.setUsage("/channel create <full-name> [<type>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (this.getInstance().getChannelManager().hasChannel(arguments[NAME])) {
                sender.sendMessage(Messages.tlErr("channelAlreadyExist", arguments[NAME]));
                return true;
            }

            final ChannelType type = arguments.length == this.getMaxArgs()
                    ? ChannelType.getType(arguments[TYPE]) : ChannelType.getDefault();

            if (type == null || type == ChannelType.CONVERSATION) {
                sender.sendMessage(Messages.tlErr("invalidType"));
                return true;
            }

            if (sender.canCreate(type)) {
                if (sender instanceof IChatter && sender.hasPermission(Permission.CREATE.formChildren("limit"))) {
                    final UUID uniqueId = ((IChatter) sender).getUniqueId();

                    for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                        if (channel.isOwner(uniqueId)) {
                            sender.sendMessage(Messages.tl("createLimit", 1));
                            return true;
                        }
                    }
                }

                try {
                    final IChannel channel = this.getInstance().getChannelManager().addChannel(arguments[NAME], type);

                    if (channel == null) {
                        sender.sendMessage(Messages.tlErr("createAlready", arguments[NAME]));
                        return true;
                    }

                    if (channel.isStandard() && sender instanceof IChatter) {
                        final IChatter chatter = (IChatter) sender;

                        channel.setOwner(chatter.getUniqueId());
                        channel.addMember(chatter);
                    }

                    if (channel.isPersist() && channel instanceof IStorageHolder) {
                        final IStorageHolder holder = (IStorageHolder) channel;

                        this.getInstance().runStorageTask(holder::save);
                    }

                    sender.sendMessage(Messages.tl("createChannel", channel.getFullName(), Messages.tlType(type)));
                    return true;
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidName"));
                    return true;
                }
            }

            sender.sendMessage(Messages.tl("createDenied", Messages.tlType(type)));
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

            for (final ChannelType type : ChannelType.values()) {
                if (type == ChannelType.CONVERSATION) {
                    continue;
                }

                if (sender.canCreate(type)) {
                    if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), arguments[TYPE])) {
                        completion.add(type.getIdentifier());
                    }
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
