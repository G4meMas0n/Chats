package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Unmute Command, extends {@link ModerateOfflineCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: April 16th, 2020
 * changed: June 19th, 2020
 */
public final class UnmuteCommand extends ModerateOfflineCommand {

    public UnmuteCommand() {
        super("unmute", 2 , 2);

        this.setDescription("Unmutes a player in a channel.");
        this.setPermission(Permission.UNMUTE.getNode());
        this.setUsage("/channel unmute <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final IOfflineChatter target,
                           @NotNull final IChannel channel) {
        final IChatter online = target instanceof IChatter ? (IChatter) target : null;

        if (!channel.isMuted(target.getUniqueId())) {
            sender.sendMessage(Messages.tl("unmuteAlready", (online != null && sender.canSee(online))
                    ? online.getDisplayName() : target.getName(), channel.getColoredName()));
            return true;
        }

        if (channel.unmuteMember(target)) {
            sender.sendMessage(Messages.tl("unmuteMember", (online != null && sender.canSee(online))
                    ? online.getDisplayName() : target.getName(), channel.getColoredName()));
            return true;
        }

        sender.sendMessage(Messages.tl("unmuteFailed", (online != null && sender.canSee(online))
                ? online.getDisplayName() : target.getName(), channel.getColoredName()));
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
                for (final UUID uniqueId : channel.getMutes()) {
                    final IOfflineChatter muted = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (muted == null) {
                        channel.setMuted(uniqueId, false);
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(muted.getName(), target)) {
                        completion.add(muted.getName());
                    }
                }
            }
        }

        Collections.sort(completion);

        return completion;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final IOfflineChatter target,
                                             @NotNull final String fullName) {
        final List<String> completion = new ArrayList<>();

        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
            if (channel.isConversation() || !channel.isMuted(target.getUniqueId())) {
                continue;
            }

            if (sender.canModerate(channel)) {
                if (StringUtil.startsWithIgnoreCase(channel.getFullName(), fullName)) {
                    completion.add(channel.getFullName());
                }
            }
        }

        Collections.sort(completion);

        return completion;
    }
}
