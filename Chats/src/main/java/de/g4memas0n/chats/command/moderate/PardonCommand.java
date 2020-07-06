package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The pardon command that allows to pardon a banned member from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class PardonCommand extends ModerateCommand {

    public PardonCommand() {
        super("pardon", 2, 2);

        this.setAliases(Collections.singletonList("unban"));
        this.setDescription("Pardons a player from a channel.");
        this.setPermission(Permission.PARDON.getNode());
        this.setUsage("/channel (pardon|unban) <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(TARGET));

            if (target == null) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canModerate(channel)) {
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

            sender.sendMessage(Messages.tl("moderateDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TARGET + 1) {
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

                        if (StringUtil.startsWithIgnoreCase(banned.getName(), input.get(TARGET))) {
                            completion.add(banned.getName());
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (input.getLength() == CHANNEL + 1) {
            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(TARGET));

            if (target == null) {
                return Collections.emptyList();
            }

            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation() || !channel.isBanned(target.getUniqueId())) {
                    continue;
                }

                if (sender.canModerate(channel)) {
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
