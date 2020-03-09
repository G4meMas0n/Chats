package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
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
 * The Focus Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: March 3rd, 2020
 */
public final class FocusCommand extends BasicPluginCommand {

    private static final String NAME = "focus";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 2;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_PASSWORD = 1;

    public FocusCommand() {
        super(NAME, Permission.CHANNEL_JOIN.getName(), MIN_ARGS, MAX_ARGS);
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

            if (chatter.canFocus(channel)) {
                if (!chatter.hasChannel(channel)) {
                    if (chatter.canJoin(channel)) {
                        if (channel.hasPassword()) {
                            if (arguments.length != this.getMaxArgs()) {
                                sender.sendMessage(Messages.tlErr("passwordMissing"));
                                return true;
                            }

                            if (!arguments[ARG_PASSWORD].equals(channel.getPassword())) {
                                sender.sendMessage(Messages.tlErr("passwordInvalid"));
                                return true;
                            }
                        }

                        if (chatter.addChannel(channel)) {
                            sender.sendMessage(Messages.tl("joinChannel", channel.getColoredName()));
                            return true;
                        }
                    }

                    sender.sendMessage(Messages.tl("joinDenied", channel.getColoredName()));
                    return true;
                }

                if (chatter.setFocus(channel)) {
                    sender.sendMessage(Messages.tl("focusChannel", channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("focusAlreadyChannel", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("focusDenied", channel.getColoredName()));
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

                for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                    if (current.isConversation() || chatter.getFocus().equals(current)) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                        if (chatter.canFocus(current)) {
                            if (!chatter.hasChannel(current) && !chatter.canJoin(current)) {
                                continue;
                            }

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
