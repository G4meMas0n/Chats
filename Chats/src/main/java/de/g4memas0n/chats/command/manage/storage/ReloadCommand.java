package de.g4memas0n.chats.command.manage.storage;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.channel.PersistChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.IStorageHolder.Type;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The reload command that reloads the complete plugin or parts of the plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ReloadCommand extends BasicCommand {

    private static final int TYPE = 0;
    private static final int STORAGE = 1;

    public ReloadCommand() {
        super("reload", 0, 2);

        this.setPermission(Permission.RELOAD.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final Type type = input.getLength() == this.getMaxArgs() ? Type.getType(input.get(TYPE)) : Type.getDefault();

            if (type == null) {
                return false;
            }

            if (sender.canReload(type)) {
                if (type == Type.ALL && input.getLength() == this.getMinArgs()) {
                    this.getInstance().reloadConfig();
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", this.getInstance().getName()));
                    return true;
                }

                if (type == Type.CHANNEL && input.getLength() == this.getMaxArgs()) {
                    final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(STORAGE));

                    if (channel == null || channel.isConversation()) {
                        throw new ChannelNotExistException(input.get(STORAGE));
                    }

                    if (channel instanceof PersistChannel) {
                        try {
                            this.getInstance().runStorageTask(((PersistChannel) channel)::load).get();

                            sender.sendMessage(tl("reloadChannel", channel.getFullName()));
                            return true;
                        } catch (ExecutionException ex) {
                            this.getInstance().getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
                        } catch (InterruptedException ex) {
                            this.getInstance().getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
                        }

                        sender.sendMessage(tl("reloadFailed", channel.getFullName()));
                        return true;
                    }

                    throw new InvalidArgumentException("channelNotPersist", channel.getFullName());
                }

                if (type == Type.CHANNEL && input.getLength() == this.getMinArgs()) {
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", tl("channels")));
                    return true;
                }

                if (type == Type.CHATTER && input.getLength() == this.getMaxArgs()) {
                    final IChatter chatter = this.getInstance().getChatterManager().getChatter(input.get(STORAGE));

                    if (chatter == null || !sender.canSee(chatter)) {
                        throw new PlayerNotFoundException(input.get(STORAGE));
                    }

                    try {
                        this.getInstance().runStorageTask(chatter::load).get();

                        sender.sendMessage(tl("reloadChatter", chatter.getDisplayName()));
                        return true;
                    } catch (ExecutionException ex) {
                        this.getInstance().getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
                    } catch (InterruptedException ex) {
                        this.getInstance().getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
                    }

                    sender.sendMessage(tl("reloadFailed", chatter.getDisplayName()));
                    return true;
                }

                if (type == Type.CHATTER && input.getLength() == this.getMinArgs()) {
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", tl("chatters")));
                    return true;
                }

                if (type == Type.CONFIG && input.getLength() == this.getMinArgs()) {
                    this.getInstance().reloadConfig();

                    sender.sendMessage(tl("reloadComplete", tl("config")));
                    return true;
                }

                return false;
            }

            sender.sendMessage(tl("reloadDenied", tl(type.getKey())));
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
                if (sender.canReload(type)) {
                    if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
                        completion.add(type.getIdentifier());
                    }
                }
            }

            return completion;
        }

        if (input.getLength() == STORAGE + 1) {
            final Type type = Type.getType(input.get(TYPE));

            if (type == null) {
                return Collections.emptyList();
            }

            if (sender.canReload(type)) {
                if (type == Type.CHANNEL) {
                    final List<String> completion = new ArrayList<>();

                    for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                        if (!channel.isPersist() || !(channel instanceof IStorageHolder)) {
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(channel.getFullName(), input.get(STORAGE))) {
                            completion.add(channel.getFullName());
                        }
                    }

                    Collections.sort(completion);

                    return completion;
                }

                if (type == Type.CHATTER) {
                    final List<String> completion = new ArrayList<>();

                    for (final IChatter chatter : this.getInstance().getChatterManager().getChatters()) {
                        if (!sender.canSee(chatter)) {
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(chatter.getName(), input.get(STORAGE))) {
                            completion.add(chatter.getName());
                        }
                    }

                    Collections.sort(completion);

                    return completion;
                }
            }
        }

        return Collections.emptyList();
    }
}
