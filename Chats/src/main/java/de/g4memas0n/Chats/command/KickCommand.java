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
 * The Kick Command, extends {@link BasicCommand}.
 *
 * @author G4meMason
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 12th, 2020
 * changed: June 19th, 2020
 */
public final class KickCommand extends ModerateOnlineCommand {

    public KickCommand() {
        super("kick", 2, 2);

        this.setDescription("Kicks a player from a channel.");
        this.setPermission(Permission.KICK.getNode());
        this.setUsage("/channel kick <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final IChatter target,
                           @NotNull final IChannel channel) {
        if (channel.isDefault()) {
            sender.sendMessage(Messages.tlErr("kickDefault"));
            return true;
        }

        if (target.equals(sender)) {
            sender.sendMessage(Messages.tlErr("kickSelf"));
            return true;
        }

        if (!channel.isMember(target)) {
            sender.sendMessage(Messages.tl("noMember", target.getDisplayName(), channel.getColoredName()));
            return true;
        }

        if (sender.canKick(target, channel)) {
            if (channel.kickMember(target)) {
                sender.sendMessage(Messages.tl("kickMember", target.getDisplayName(), channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("kickFailed", target.getDisplayName(), channel.getColoredName()));
            return true;
        }

        sender.sendMessage(Messages.tl("kickDenied", target.getDisplayName(), channel.getColoredName()));
        return true;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String target) {
        final List<String> completion = new ArrayList<>();

        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
            if (channel.isConversation() || channel.isDefault()) {
                continue;
            }

            if (sender.canModerate(channel)) {
                for (final IChatter member : channel.getMembers()) {
                    if (!sender.canSee(member) || member.equals(sender)) {
                        continue;
                    }

                    if (sender.canKick(member, channel)) {
                        if (StringUtil.startsWithIgnoreCase(member.getName(), target)) {
                            completion.add(member.getName());
                        }
                    }
                }
            }
        }

        Collections.sort(completion);

        return completion;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final IChatter target,
                                             @NotNull final String fullName) {
        final List<String> completion = new ArrayList<>();

        for (final IChannel channel : target.getChannels()) {
            if (channel.isConversation() || channel.isDefault()) {
                continue;
            }

            if (sender.canModerate(channel) && sender.canKick(target, channel)) {
                if (StringUtil.startsWithIgnoreCase(channel.getFullName(), fullName)) {
                    completion.add(channel.getFullName());
                }
            }
        }

        Collections.sort(completion);

        return completion;
    }
}
