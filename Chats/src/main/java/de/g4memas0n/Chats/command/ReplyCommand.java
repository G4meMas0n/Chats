package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.ConversationChannel;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.ChatRunnable;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * The Reply Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: March 4th, 2020
 */
public final class ReplyCommand extends BasicPluginCommand {

    private static final String NAME = "reply";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = -1;

    private static final int ARG_MESSAGE = 0;

    public ReplyCommand() {
        super(NAME, Permission.CHATTER_MSG.getName(), MIN_ARGS, MAX_ARGS, Collections.singletonList("r"));
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
            final IChatter partner = chatter.getLastPartner();

            if (partner == null || !chatter.getPlayer().canSee(partner.getPlayer())) {
                sender.sendMessage(Messages.tlErr("noLastPartner"));
                return true;
            }

            if (chatter.canMessage(partner.getPlayer())) {
                IChannel channel = this.getInstance().getChannelManager()
                        .getChannel(IChannel.buildConversationName(chatter, partner));

                if (channel == null || !channel.isConversation()) {
                    channel = new ConversationChannel(this.getInstance().getChannelManager(),
                            this.getInstance().getFormatter(), chatter, partner);

                    this.getInstance().getChannelManager().addChannel(channel);
                }

                if (arguments.length == this.getMinArgs()) {
                    if (chatter.setFocus(channel)) {
                        sender.sendMessage(Messages.tl("focusConversation", partner.getPlayer().getDisplayName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("focusAlreadyConversation",
                            partner.getPlayer().getDisplayName()));
                    return true;
                }

                final Runnable runnable = new ChatRunnable(channel, chatter, copyMessage(arguments, ARG_MESSAGE));

                this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);
                return true;
            }

            sender.sendMessage(Messages.tl("msgDenied", partner.getPlayer().getName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        return Collections.emptyList();
    }
}
