package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chat.ChatRunnable;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.BlockCommandSender;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Chat Command TabExecutor, extends {@link ChatsPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: February 3rd, 2020
 */
public final class ChatCommand extends ChatsPluginCommand {

    private static final String NAME = "chat";
    private static final int MIN_ARGS = 2;
    private static final int MAX_ARGS = -1;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_MSG = 1;

    public ChatCommand() {
        super(NAME, Permission.CHANNEL_SPEAK.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] arguments) {
        if (sender instanceof BlockCommandSender || sender instanceof ConsoleCommandSender) {
            sender.sendMessage(""); //TODO: Add localized 'command_illegalAccess' message.
            return true;
        }

        if (!sender.hasPermission(this.getPermission())) {
            sender.sendMessage(this.getPermissionMessage());
            return true;
        }

        if (this.argsInRange(arguments.length)) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(""); //TODO: Add localized 'channel_notExist' message.
                return true;
            }

            if (chatter.hasChannel(channel)) {
                if (chatter.canSpeak(channel)) {
                    final Runnable runnable = new ChatRunnable(channel, chatter, getMessage(arguments, ARG_MSG));

                    this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);

                    return true;
                }

                sender.sendMessage(""); //TODO: Add localized 'chatter_chatDenied' message.
                return true;
            }

            sender.sendMessage(""); //TODO: All localized chatter_chatNotJoined' message.
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                               @NotNull final Command command,
                                               @NotNull final String alias,
                                               @NotNull final String[] arguments) {
        final List<String> completion = new ArrayList<>();

        if (sender instanceof BlockCommandSender || sender instanceof ConsoleCommandSender) {
            return completion;
        }

        if (!sender.hasPermission(this.getPermission())) {
            return completion;
        }

        if (this.argsInRange(arguments.length)) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            if (arguments.length == ARG_CHANNEL + 1) {
                for (final IChannel current : chatter.getChannels()) {
                    if (current.isConversation()) {
                        continue;
                    }

                    if (chatter.canSpeak(current)) {
                        if (current.getFullName().contains(arguments[ARG_CHANNEL])) {
                            completion.add(current.getFullName());
                        }
                    }
                }

                Collections.sort(completion);
            }
        }

        return completion;
    }
}
