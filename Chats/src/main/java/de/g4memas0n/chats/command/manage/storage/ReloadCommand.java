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
import de.g4memas0n.chats.util.type.StorageType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlType;

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

        this.setDescription("Reloads this plugin.");
        this.setPermission(Permission.RELOAD.getNode());
        this.setUsage("/chats reload [(all|channel [<channel>]| chatter [<player>]|config)]");
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final StorageType type = input.getLength() == this.getMinArgs()
                    ? StorageType.getDefault() : StorageType.getType(input.get(TYPE));

            if (type == null) {
                return false;
            }

            if (sender.canReload(type)) {
                if (input.getLength() == this.getMaxArgs()) {
                    if (type == StorageType.CHANNEL) {
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

                    if (type == StorageType.CHATTER) {
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

                    return false;
                }

                if (type == StorageType.ALL) {
                    this.getInstance().reloadConfig();
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", this.getInstance().getName()));
                    return true;
                }

                if (type == StorageType.CHANNEL) {
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", tl("channels")));
                    return true;
                }

                if (type == StorageType.CHATTER) {
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(tl("reloadComplete", tl("chatters")));
                    return true;
                }

                if (type == StorageType.CONFIG) {
                    this.getInstance().reloadConfig();

                    sender.sendMessage(tl("reloadComplete", tl("config")));
                    return true;
                }

                return false;
            }

            sender.sendMessage(tl("reloadDenied", tlType(type)));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final StorageType type : StorageType.values()) {
                if (sender.canReload(type) && StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
                    completion.add(type.getIdentifier());
                }
            }

            return completion;
        }

        if (input.getLength() == STORAGE + 1) {
            final StorageType type = StorageType.getType(input.get(TYPE));

            if (type == null) {
                return Collections.emptyList();
            }

            if (sender.canReload(type)) {
                if (type == StorageType.CHANNEL) {
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

                if (type == StorageType.CHATTER) {
                    final List<String> completion = new ArrayList<>();

                    for (final IChatter chatter : this.getInstance().getChatterManager().getChatters()) {
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
