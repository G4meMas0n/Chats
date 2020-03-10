package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.ChatRunnable;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Chat Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: March 10th, 2020
 */
public final class ChatCommand extends BasicPluginCommand {

    private static final String NAME = "chat";
    private static final int MIN_ARGS = 2;
    private static final int MAX_ARGS = -1;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_MSG = 1;

    public ChatCommand() {
        super(NAME, Permission.CHANNEL_SPEAK.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof Player)) {
                return false;
            }

            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[ARG_CHANNEL]));
                return true;
            }

            if (chatter.hasChannel(channel)) {
                if (channel.isMuted(chatter)) {
                    sender.sendMessage(Messages.tl("muted", channel.getColoredName()));
                }

                if (chatter.canSpeak(channel)) {
                    final Runnable runnable = new ChatRunnable(channel, chatter, copyMessage(arguments, ARG_MSG));

                    this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);

                    return true;
                }

                sender.sendMessage(Messages.tl("chatDenied", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tlErr("leaveAlready", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == ARG_CHANNEL + 1) {
                if (!(sender instanceof Player)) {
                    return Collections.emptyList();
                }

                final List<String> completion = new ArrayList<>();
                final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

                for (final IChannel current : chatter.getChannels()) {
                    if (current.isConversation()) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                        if (chatter.canSpeak(current)) {
                            completion.add(current.getFullName());
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
