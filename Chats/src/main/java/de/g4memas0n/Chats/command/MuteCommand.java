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
 * The Mute Command, extends {@link ModerateOnlineCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 12th, 2020
 * changed: June 19th, 2020
 */
public final class MuteCommand extends ModerateOnlineCommand {

    public MuteCommand() {
        super("mute", 2 , 2);

        this.setDescription("Mutes a player in a channel.");
        this.setPermission(Permission.MUTE.getNode());
        this.setUsage("/channel mute <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final IChatter target,
                           @NotNull final IChannel channel) {
        if (!channel.isMember(target)) {
            sender.sendMessage(Messages.tl("noMember", target.getDisplayName(), channel.getColoredName()));
            return true;
        }

        if (channel.isMuted(target.getUniqueId())) {
            sender.sendMessage(Messages.tl("muteAlready", target.getDisplayName(), channel.getColoredName()));
            return true;
        }

        if (sender.canMute(target, channel)) {
            if (channel.muteMember(target)) {
                sender.sendMessage(Messages.tl("muteMember", target.getDisplayName(), channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("muteFailed", target.getDisplayName(), channel.getColoredName()));
            return true;
        }

        sender.sendMessage(Messages.tl("muteDenied", target.getDisplayName(), channel.getColoredName()));
        return true;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String target) {
        final List<String> completion = new ArrayList<>();

        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
            if (channel.isConversation()) {
                continue;
            }

            if (sender.canModerate(channel)) {
                for (final IChatter member : channel.getMembers()) {
                    if (!sender.canSee(member) || member.equals(sender) || channel.isMuted(member.getUniqueId())) {
                        continue;
                    }

                    if (sender.canMute(member, channel)) {
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
            if (channel.isConversation() || channel.isMuted(target.getUniqueId())) {
                continue;
            }

            if (sender.canModerate(channel) && sender.canMute(target, channel)) {
                if (StringUtil.startsWithIgnoreCase(channel.getFullName(), fullName)) {
                    completion.add(channel.getFullName());
                }
            }
        }

        Collections.sort(completion);

        return completion;
    }
}
