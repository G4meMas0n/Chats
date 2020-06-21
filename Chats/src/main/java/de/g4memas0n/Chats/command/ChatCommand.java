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
 * The Chat Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: June 19th, 2020
 */
public final class ChatCommand extends BasicCommand {

    private static final int CHANNEL = 0;
    private static final int MESSAGE = 1;

    public ChatCommand() {
        super("chat", 2, -1);

        this.setDescription("Sends a message in a channel without changing the focused channel.");
        this.setPermission(Permission.SPEAK.getNode());
        this.setUsage("/chat <channel> <message>");
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

            if (chatter.hasChannel(channel)) {
                if (sender.canSpeak(channel)) {
                    if (channel.isMuted(chatter.getUniqueId())) {
                        sender.sendMessage(Messages.tl("mutedMember", channel.getColoredName()));
                        return true;
                    }

                    final StringBuilder message = new StringBuilder();

                    for (int i = MESSAGE; i < arguments.length; i++) {
                        message.append(arguments[i]).append(" ");
                    }

                    this.getInstance().runSyncTask(() -> channel.performChat(chatter, message.toString().trim()));
                    return true;
                }

                sender.sendMessage(Messages.tl("chatDenied", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("leaveAlready", channel.getColoredName()));
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

            for (final IChannel channel : chatter.getChannels()) {
                if (channel.isConversation() || channel.isMuted(chatter.getUniqueId())) {
                    continue;
                }

                if (sender.canSpeak(channel)) {
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
