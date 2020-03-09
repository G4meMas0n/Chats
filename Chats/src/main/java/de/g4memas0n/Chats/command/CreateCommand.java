package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.PersistChannel;
import de.g4memas0n.Chats.channel.StandardChannel;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * The Create Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: March 3rd, 2020
 */
public final class CreateCommand extends BasicCommand {

    private static final String NAME = "create";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 2;

    private static final String CREATE_P = "create-p";
    private static final String CREATE_S = "create-s";

    private static final int ARG_NAME = 0;
    private static final int ARG_TYPE = 1;

    public CreateCommand() {
        super(NAME, Permission.CHANNEL_CREATE.getName(), MIN_ARGS, MAX_ARGS, Arrays.asList(CREATE_P, CREATE_S));
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);
            final String fullName = arguments[ARG_NAME];

            if (fullName.isEmpty()) {
                sender.sendMessage(Messages.tlErr("invalidName"));
                return true;
            }

            if (this.getInstance().getChannelManager().hasChannel(fullName)) {
                sender.sendMessage(Messages.tlErr("channelAlreadyExist", fullName));
                return true;
            }

            final ChannelType type = this.getType(alias, arguments);

            if (type == null || type == ChannelType.CONVERSATION) {
                return false;
            }

            if (permissible.canCreate(type)) {
                IChannel channel;

                if (type == ChannelType.PERSIST) {
                    final IStorageFile storage = this.getInstance().getChannelManager().getStorageFile(fullName);

                    channel = new PersistChannel(this.getInstance().getFormatter(), storage);
                } else {
                    channel = new StandardChannel(this.getInstance().getFormatter(), fullName);
                }

                if (this.getInstance().getChannelManager().addChannel(channel)) {
                    sender.sendMessage(Messages.tl("createChannel", channel.getFullName(), type.getIdentifier()));
                    return true;
                }

                sender.sendMessage(Messages.tlErr("channelAlreadyExist", fullName));
                return true;
            }

            sender.sendMessage(Messages.tl("createDenied", type.getIdentifier()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (alias.equalsIgnoreCase(CREATE_P) || alias.equalsIgnoreCase(CREATE_S)) {
                return Collections.emptyList();
            }

            if (arguments.length == this.getMaxArgs()) {
                final List<String> completion = new ArrayList<>();
                final IPermissible permissible = this.getPermissible(sender);

                for (final ChannelType current : ChannelType.values()) {
                    if (current == ChannelType.CONVERSATION) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getIdentifier(), arguments[ARG_TYPE])) {
                        if (permissible.canCreate(current)) {
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

    private @Nullable ChannelType getType(@NotNull final String alias, @NotNull final String[] arguments) {
        if (arguments.length == this.getMaxArgs()) {
            return alias.equalsIgnoreCase(this.getName()) ? ChannelType.getType(arguments[ARG_TYPE]) : null;
        } else if (alias.equalsIgnoreCase(CREATE_P)) {
            return ChannelType.PERSIST;
        } else if (alias.equalsIgnoreCase(CREATE_S)) {
            return ChannelType.STANDARD;
        } else {
            return ChannelType.STANDARD;
        }
    }
}
