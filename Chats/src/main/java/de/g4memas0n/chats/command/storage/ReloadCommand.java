package de.g4memas0n.chats.command.storage;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import de.g4memas0n.chats.util.logging.Log;
import de.g4memas0n.chats.util.type.StorageType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * The reload command that reloads the complete plugin or parts of the plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 13th, 2020
 * changed: July 3rd, 2020
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
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final StorageType type = input.getLength() == this.getMinArgs() ? StorageType.getDefault() : StorageType.getType(input.get(TYPE));

            if (type == null) {
                return false;
            }

            if (sender.canReload(type)) {
                if (input.getLength() == this.getMaxArgs()) {
                    if (type.equals(StorageType.CHANNEL)) {
                        final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(STORAGE));

                        if (channel == null || channel.isConversation()) {
                            throw new ChannelNotExistException(input.get(STORAGE));
                        }

                        if (channel.isPersist() && channel instanceof IStorageHolder) {
                            final Future<?> task = this.getInstance().runStorageTask(((IStorageHolder) channel)::load);

                            try {
                                task.get();

                                sender.sendMessage(Messages.tl("reloadChannel", channel.getFullName()));
                                return true;
                            } catch (ExecutionException ex) {
                                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
                            } catch (InterruptedException ex) {
                                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
                            }

                            sender.sendMessage(Messages.tl("reloadFailed", channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tlErr("channelNotPersist", channel.getFullName()));
                        return true;
                    } else if (type.equals(StorageType.CHATTER)) {
                        final IChatter chatter = this.getInstance().getChatterManager().getChatter(input.get(STORAGE));

                        if (chatter == null) {
                            throw new PlayerNotFoundException(input.get(STORAGE));
                        }

                        final Future<?> task = this.getInstance().runStorageTask(chatter::load);

                        try {
                            task.get();

                            sender.sendMessage(Messages.tl("reloadChatter", chatter.getName()));
                            return true;
                        } catch (ExecutionException ex) {
                            Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
                        } catch (InterruptedException ex) {
                            Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
                        }

                        sender.sendMessage(Messages.tl("reloadFailed", chatter.getName()));
                        return true;
                    }

                    return false;
                }

                if (type.equals(StorageType.ALL)) {
                    this.getInstance().reloadConfig();
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(Messages.tl("reloadComplete", this.getInstance().getName()));
                    return true;
                }

                if (type.equals(StorageType.CHANNEL)) {
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(Messages.tl("reloadComplete", Messages.tlType(type)));
                    return true;
                }

                if (type.equals(StorageType.CHATTER)) {
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(Messages.tl("reloadComplete", Messages.tlType(type)));
                    return true;
                }

                if (type.equals(StorageType.CONFIG)) {
                    this.getInstance().reloadConfig();

                    sender.sendMessage(Messages.tl("reloadComplete", Messages.tlType(type)));
                    return true;
                }

                return false;
            }

            sender.sendMessage(Messages.tl("reloadDenied", Messages.tlType(type)));
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
            final StorageType type = StorageType.getType(input.get(STORAGE));

            if (type == null) {
                return Collections.emptyList();
            }

            if (sender.canReload(type)) {
                if (type.equals(StorageType.CHANNEL)) {
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

                if (type.equals(StorageType.CHATTER)) {
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
