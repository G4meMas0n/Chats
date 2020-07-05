package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The focus command that allows to focus a new channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 11th, 2020
 * changed: July 3rd, 2020
 */
public final class FocusCommand extends ChatterCommand {

    private static final int CHANNEL = 0;
    private static final int PASSWORD = 1;

    public FocusCommand() {
        super("focus", 1, 2);

        this.setDescription("Focuses a channel.");
        this.setPermission(Permission.FOCUS.getNode());
        this.setUsage("/focus <channel> [<password>]");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canFocus(channel)) {
                if (!sender.hasChannel(channel)) {
                    if (!sender.canJoin(channel)) {
                        sender.sendMessage(Messages.tl("joinDenied", channel.getColoredName()));
                        return true;
                    }

                    if (channel.isBanned(sender.getUniqueId()) && !sender.hasPermission(Permission.BAN.getChildren("bypass"))) {
                        sender.sendMessage(Messages.tl("bannedMember", channel.getColoredName()));
                        return true;
                    }

                    if (channel.hasPassword()) {
                        if (input.getLength() != this.getMaxArgs()) {
                            sender.sendMessage(Messages.tl("passwordMissing", channel.getColoredName()));
                            return true;
                        }

                        if (!input.get(PASSWORD).equals(channel.getPassword())) {
                            sender.sendMessage(Messages.tlErr("passwordInvalid"));
                            return true;
                        }
                    } else {
                        if (input.getLength() != this.getMinArgs()) {
                            sender.sendMessage(Messages.tl("noPassword", channel.getColoredName()));
                            return true;
                        }
                    }

                    if (sender.joinChannel(channel)) {
                        sender.sendMessage(Messages.tl("joinChannel", channel.getColoredName()));
                    }
                }

                if (sender.setFocus(channel)) {
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
    public @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation() || channel.equals(sender.getFocus())) {
                    continue;
                }

                if (sender.canFocus(channel)) {
                    if (!sender.hasChannel(channel)) {
                        if (!sender.canJoin(channel)) {
                            continue;
                        }

                        if (channel.isBanned(sender.getUniqueId()) && !sender.hasPermission(Permission.BAN.getChildren("bypass"))) {
                            continue;
                        }
                    }

                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), input.get(CHANNEL))) {
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
