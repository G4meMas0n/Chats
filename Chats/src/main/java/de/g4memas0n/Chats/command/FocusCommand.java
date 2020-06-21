package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Focus Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: June 19th, 2020
 */
public final class FocusCommand extends BasicCommand {

    private static final int CHANNEL = 0;
    private static final int PASSWORD = 1;

    public FocusCommand() {
        super("focus", 1, 2);

        this.setDescription("Focuses a channel.");
        this.setPermission(Permission.FOCUS.getNode());
        this.setUsage("/focus <channel> [<password>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter chatter = (IChatter) sender;
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canFocus(channel)) {
                if (!chatter.hasChannel(channel)) {
                    if (!sender.canJoin(channel)) {
                        sender.sendMessage(Messages.tl("joinDenied", channel.getColoredName()));
                        return true;
                    }

                    if (channel.isBanned(chatter.getUniqueId())) {
                        sender.sendMessage(Messages.tl("bannedMember", channel.getColoredName()));
                        return true;
                    }

                    if (channel.hasPassword()) {
                        if (arguments.length != this.getMaxArgs()) {
                            sender.sendMessage(Messages.tl("passwordMissing", channel.getColoredName()));
                            return true;
                        }

                        if (!arguments[PASSWORD].equals(channel.getPassword())) {
                            sender.sendMessage(Messages.tlErr("passwordInvalid"));
                            return true;
                        }
                    } else {
                        if (arguments.length != this.getMinArgs()) {
                            sender.sendMessage(Messages.tl("noPassword", channel.getColoredName()));
                            return true;
                        }
                    }

                    if (chatter.joinChannel(channel)) {
                        sender.sendMessage(Messages.tl("joinChannel", channel.getColoredName()));
                    }
                }

                if (chatter.setFocus(channel)) {
                    sender.sendMessage(Messages.tl("focusChannel", channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("focusChannelAlready", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("focusDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == CHANNEL + 1) {
            if (!(sender instanceof IChatter)) {
                return Collections.emptyList();
            }

            final IChatter chatter = (IChatter) sender;
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation() || channel.equals(chatter.getFocus())) {
                    continue;
                }

                if (sender.canFocus(channel)) {
                    if (!chatter.hasChannel(channel) && !sender.canJoin(channel)) {
                        continue;
                    }

                    if (channel.isBanned(chatter.getUniqueId())) {
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), arguments[CHANNEL])) {
                        completion.add(channel.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
