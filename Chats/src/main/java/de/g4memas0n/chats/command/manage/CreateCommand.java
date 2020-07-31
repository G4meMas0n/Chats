package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.channel.IChannel.Type;
import de.g4memas0n.chats.channel.PersistChannel;
import de.g4memas0n.chats.channel.StandardChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.TypeNotAvailableException;
import de.g4memas0n.chats.command.TypeNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The create command that allows to create a new channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class CreateCommand extends BasicCommand {

    private static final int NAME = 0;
    private static final int TYPE = 1;

    public CreateCommand() {
        super("create", 2, 2);

        this.setPermission(Permission.CREATE.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter && !sender.hasPermission(Permission.CREATE.getChildren("unlimited"))) {
            final IChatter creator = (IChatter) sender;

            final long owning = this.getInstance().getChannelManager().getChannels().stream()
                    .filter(channel -> channel.isOwner(creator.getUniqueId())).count();

            return owning >= this.getInstance().getSettings().getCreateLimit();
        }

        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final String fullName = input.get(NAME);

            if (!fullName.matches(StandardChannel.REGEX) || fullName.equalsIgnoreCase(StandardChannel.INVALID)) {
                throw new InvalidArgumentException("invalidName");
            }

            if (this.getInstance().getChannelManager().hasChannel(fullName)) {
                throw new InvalidArgumentException("channelAlreadyExist", fullName);
            }

            final Type type = Type.getType(input.get(TYPE));

            if (type == null || type == Type.CONVERSATION) {
                throw new TypeNotFoundException(input.get(TYPE));
            }

            if (sender.canCreate(type)) {
                if (type == Type.PERSIST) {
                    final PersistChannel channel = this.getInstance().getChannelManager().addPersist(fullName);

                    if (channel == null) {
                        throw new InvalidArgumentException("channelAlreadyExist", fullName);
                    }

                    this.getInstance().runStorageTask(channel::save);

                    sender.sendMessage(tl("createChannel", channel.getFullName(), tl("persist")));
                    return true;
                }

                if (type == Type.STANDARD) {
                    final IChatter creator = sender instanceof IChatter ? (IChatter) sender : null;

                    if (creator != null && !creator.hasPermission(Permission.CREATE.getChildren("unlimited"))) {
                        final long owning = this.getInstance().getChannelManager().getChannels().stream()
                                .filter(channel -> channel.isOwner(creator.getUniqueId())).count();

                        if (owning >= this.getInstance().getSettings().getCreateLimit()) {
                            sender.sendMessage(tl("createLimit", this.getInstance().getSettings().getCreateLimit()));
                            return true;
                        }
                    }

                    final StandardChannel channel = this.getInstance().getChannelManager().addStandard(fullName);

                    if (channel == null) {
                        throw new InvalidArgumentException("channelAlreadyExist", fullName);
                    }

                    if (creator != null) {
                        channel.setOwner(creator.getUniqueId());
                        channel.addMember(creator, true);
                    }

                    sender.sendMessage(tl("createChannel", channel.getFullName(), tl("standard")));
                    return true;
                }

                throw new TypeNotAvailableException(type);
            }

            sender.sendMessage(tl("createDenied", tl(type.getKey())));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final Type type : Type.values()) {
                if (type == Type.CONVERSATION) {
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
