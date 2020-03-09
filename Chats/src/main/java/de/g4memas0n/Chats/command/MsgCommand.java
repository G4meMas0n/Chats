package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.ConversationChannel;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.ChatRunnable;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * The Msg Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * changed: March 4th, 2020
 */
public final class MsgCommand extends BasicPluginCommand {

    private static final String NAME = "msg";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = -1;

    private static final int ARG_PARTNER = 0;
    private static final int ARG_MESSAGE = 1;

    public MsgCommand() {
        super(NAME, Permission.CHATTER_MSG.getName(), MIN_ARGS, MAX_ARGS, Arrays.asList("pm", "tell", "whisper", "w"));
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

            if (chatter.getPlayer().getName().equalsIgnoreCase(arguments[ARG_PARTNER])) {
                sender.sendMessage(Messages.tlErr("msgSelf"));
                return true;
            }

            final Player target = this.getInstance().getServer().getPlayer(arguments[ARG_PARTNER]);

            if (target == null || !chatter.getPlayer().canSee(target)) {
                sender.sendMessage(Messages.tlErr("playerNotFound", arguments[ARG_PARTNER]));
                return true;
            }

            if (chatter.canMessage(target)) {
                final IChatter partner = this.getInstance().getChatterManager().getChatter(target);

                IChannel channel = this.getInstance().getChannelManager()
                        .getChannel(IChannel.buildConversationName(chatter, partner));

                if (channel == null || !channel.isConversation()) {
                    channel = new ConversationChannel(this.getInstance().getChannelManager(),
                            this.getInstance().getFormatter(), chatter, partner);

                    this.getInstance().getChannelManager().addChannel(channel);
                }

                if (arguments.length == this.getMinArgs()) {
                    if (chatter.setFocus(channel)) {
                        sender.sendMessage(Messages.tl("focusConversation", target.getDisplayName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("focusAlreadyConversation", target.getDisplayName()));
                    return true;
                }

                final Runnable runnable = new ChatRunnable(channel, chatter, copyMessage(arguments, ARG_MESSAGE));

                this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);
                return true;
            }

            sender.sendMessage(Messages.tl("msgDenied", target.getName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMinArgs()) {
                if (!(sender instanceof Player)) {
                    return Collections.emptyList();
                }

                final List<String> completion = new ArrayList<>();
                final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

                for (final Player current : this.getInstance().getServer().getOnlinePlayers()) {
                    if (current.equals(chatter.getPlayer())) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getName(), arguments[ARG_PARTNER])) {
                        if (chatter.canMessage(current)) {
                            completion.add(current.getName());
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
