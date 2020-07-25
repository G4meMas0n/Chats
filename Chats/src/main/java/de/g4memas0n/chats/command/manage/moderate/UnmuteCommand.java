package de.g4memas0n.chats.command.manage.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The unmute command that allows to unmutes a player in a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class UnmuteCommand extends ModerateCommand {

    public UnmuteCommand() {
        super("unmute", 2 , 2);

        this.setDescription("Unmutes a player in a channel.");
        this.setPermission(Permission.MUTE.getNode());
        this.setUsage("/channel unmute <player> <channel>");
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

                if (!channel.isMuted(target.getUniqueId())) {
                    sender.sendMessage(tl("unmuteAlready", (online != null && sender.canSee(online))
                            ? online.getDisplayName() : target.getName(), channel.getColoredName()));
                    return true;
                }

                if (channel.unmuteMember(target)) {
                    sender.sendMessage(tl("unmuteMember", (online != null && sender.canSee(online))
                            ? online.getDisplayName() : target.getName(), channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(tl("unmuteFailed", (online != null && sender.canSee(online))
                        ? online.getDisplayName() : target.getName(), channel.getColoredName()));
                return true;
            }

            sender.sendMessage(tl("moderateDenied", channel.getColoredName()));
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
                    for (final UUID uniqueId : channel.getMutes()) {
                        final IOfflineChatter muted = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                        if (muted == null) {
                            channel.setMuted(uniqueId, false);
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(muted.getName(), input.get(TARGET))) {
                            completion.add(muted.getName());
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
                if (channel.isConversation() || !channel.isMuted(target.getUniqueId())) {
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
