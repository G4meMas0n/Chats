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
 * The Unban Command, extends {@link ModerateOfflineCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: April 8th, 2020
 * changed: June 19th, 2020
 */
public final class PardonCommand extends ModerateOfflineCommand {

    public PardonCommand() {
        super("pardon", 2, 2);

        this.setAliases(Collections.singletonList("unban"));
        this.setDescription("Pardons a player from a channel.");
        this.setPermission(Permission.PARDON.getNode());
        this.setUsage("/channel (pardon|unban) <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final IOfflineChatter target,
                           @NotNull final IChannel channel) {
        final IChatter online = target instanceof IChatter ? (IChatter) target : null;

        if (!channel.isBanned(target.getUniqueId())) {
            sender.sendMessage(Messages.tl("pardonAlready", (online != null && sender.canSee(online))
                    ? online.getDisplayName() : target.getName(), channel.getColoredName()));
            return true;
        }

        if (channel.pardonMember(target)) {
            sender.sendMessage(Messages.tl("pardonMember", (online != null && sender.canSee(online))
                    ? online.getDisplayName() : target.getName(), channel.getColoredName()));
            return true;
        }

        sender.sendMessage(Messages.tl("pardonFailed", (online != null && sender.canSee(online))
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
                for (final UUID uniqueId : channel.getBans()) {
                    final IOfflineChatter banned = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (banned == null) {
                        channel.setBanned(uniqueId, false);
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(banned.getName(), target)) {
                        completion.add(banned.getName());
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
            if (channel.isConversation() || !channel.isBanned(target.getUniqueId())) {
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
