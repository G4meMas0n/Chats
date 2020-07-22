package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlErr;
import static de.g4memas0n.chats.messaging.Messages.tlType;

/**
 * The create command that allows to create a new channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class CreateCommand extends BasicCommand {

    /**
     * Might be replaced with a configurable limit in the future.
     */
    private static final int LIMIT = 1;

    private static final int NAME = 0;
    private static final int TYPE = 1;

    public CreateCommand() {
        super("create", 1, 2);

        this.setDescription("Creates a new channel.");
        this.setPermission(Permission.CREATE.getNode());
        this.setUsage("/channel create <full-name> [<type>]");
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter && !sender.hasPermission(Permission.CREATE.getChildren("unlimited"))) {
            final IChatter creator = (IChatter) sender;
            final Set<IChannel> owning = new HashSet<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (channel.isOwner(creator.getUniqueId())) {
                    owning.add(channel);
                }
            }

            return owning.size() >= LIMIT;
        }

        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (this.getInstance().getChannelManager().hasChannel(input.get(NAME))) {
                sender.sendMessage(tlErr("channelAlreadyExist", input.get(NAME)));
                return true;
            }

            final ChannelType type = input.getLength() == this.getMaxArgs()
                    ? ChannelType.getType(input.get(TYPE)) : ChannelType.getDefault();

            if (type == null || type == ChannelType.CONVERSATION) {
                throw new InvalidArgumentException("invalidType", input.get(TYPE));
            }

            if (sender.canCreate(type)) {
                final IChatter creator = sender instanceof IChatter ? (IChatter) sender : null;

                if (creator != null && !sender.hasPermission(Permission.CREATE.getChildren("unlimited"))) {
                    final Set<IChannel> owning = new HashSet<>();

                    for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                        if (channel.isConversation()) {
                            continue;
                        }

                        if (channel.isOwner(creator.getUniqueId())) {
                            owning.add(channel);
                        }
                    }

                    if (owning.size() >= LIMIT) {
                        sender.sendMessage(tl("createLimit", 1));
                        return true;
                    }
                }

                try {
                    final IChannel channel = this.getInstance().getChannelManager().addChannel(input.get(NAME), type);

                    if (channel == null) {
                        sender.sendMessage(tlErr("createAlready", input.get(NAME)));
                        return true;
                    }

                    if (channel.isStandard() && creator != null) {
                        channel.setOwner(creator.getUniqueId());
                        channel.addMember(creator);

                        sender.sendMessage(tl("createChannel", channel.getFullName(), tlType(type)));
                        sender.sendMessage(tl("joinChannel", channel.getColoredName()));
                        return true;
                    }

                    if (channel.isPersist() && channel instanceof IStorageHolder) {
                        final IStorageHolder holder = (IStorageHolder) channel;

                        this.getInstance().runStorageTask(holder::save);
                    }

                    sender.sendMessage(tl("createChannel", channel.getFullName(), tlType(type)));
                    return true;
                } catch (IllegalArgumentException ex) {
                    throw new InvalidArgumentException("invalidName", input.get(NAME));
                }
            }

            sender.sendMessage(tl("createDenied", tlType(type)));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final ChannelType type : ChannelType.values()) {
                if (type == ChannelType.CONVERSATION) {
                    continue;
                }

                if (sender.canCreate(type)) {
                    if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
                        completion.add(type.getIdentifier());
                    }
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
