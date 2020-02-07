package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.ConversationChannel;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chat.ConversationRunnable;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.BlockCommandSender;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * The Reply Command TabExecutor, extends {@link ChatsPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: February 3rd, 2020
 */
public final class ReplyCommand extends ChatsPluginCommand {

    private static final String NAME = "reply";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = -1;

    private static final int ARG_MESSAGE = 0;

    public ReplyCommand() {
        super(NAME, Permission.CHATTER_MSG.getName(), MIN_ARGS, MAX_ARGS);
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
            sender.sendMessage(""); //TODO: Add localized 'command_permissionMessage' message.
            return true;
        }

        if (this.argsInRange(arguments.length)) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            final IChatter lastPartner = chatter.getLastPartner();

            if (lastPartner == null || !chatter.getPlayer().canSee(lastPartner.getPlayer())) {
                sender.sendMessage(""); //TODO: Add localized 'chatter_msgNoLastPartner' message.
                return true;
            }

            if (chatter.canMessage(lastPartner.getPlayer())) {
                if (arguments.length == this.getMinArgs()) {
                    IChannel channel = this.getInstance().getChannelManager().getChannel(
                            ConversationChannel.buildName(chatter, lastPartner));

                    if (channel == null) {
                        channel = this.getInstance().getChannelManager().createConversation(chatter, lastPartner);

                        // This should never be true, because there is no channel when the channel manager return null.
                        if (!this.getInstance().getChannelManager().addChannel(channel)) {
                            sender.sendMessage(""); //TODO: Add localized 'command_performUnable' message.
                            return true;
                        }
                    }

                    // This should never be true, because conversation channels should have a another name regex.
                    if (!channel.isConversation()) {
                        sender.sendMessage(""); //TODO: Add localized 'command_performUnable' message.
                        return true;
                    }

                    if (chatter.setFocus(channel)) {
                        sender.sendMessage(""); //TODO: Add localized 'command_msgFocusPartner' message.
                        return true;
                    }

                    sender.sendMessage(""); //TODO: Add localized 'command_msgFocusAlready' message.
                    return true;
                }

                final String message = this.getMessage(arguments, ARG_MESSAGE);
                final Runnable runnable = new ConversationRunnable(
                        this.getInstance().getChannelManager().getPerformer(), chatter, lastPartner, message);

                this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);
                return true;
            }
        }

        return false;
    }

    @Override
    public List<String> onTabComplete(@NotNull final CommandSender sender,
                                      @NotNull final Command command,
                                      @NotNull final String alias,
                                      @NotNull final String[] arguments) {
        return new ArrayList<>();
    }
}
