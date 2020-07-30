package de.g4memas0n.chats.command.manage.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The ban command that allows to ban a player from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class BanCommand extends ModerateCommand {

    public BanCommand() {
        super("ban", 2, 2);

        this.setPermission(Permission.BAN.getNode());
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

            if (target == null || !sender.canSee(target)) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            if (target.equals(sender)) {
                throw new InvalidArgumentException("banSelf");
            }

            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canModerate(channel)) {
                if (channel.isDefault()) {
                    throw new InvalidArgumentException("banDefault");
                }

                if (channel.isBanned(target.getUniqueId())) {
                    sender.sendMessage(tl("banAlready", target.getDisplayName(), channel.getColoredName()));
                    return true;
                }

                if (!channel.isMember(target)) {
                    sender.sendMessage(tl("noMember", target.getDisplayName(), channel.getColoredName()));
                    return true;
                }

                if (sender.canBan(target, channel)) {
                    if (channel.banMember(target)) {
                        sender.sendMessage(tl("banMember", target.getDisplayName(), channel.getColoredName()));
                        return true;
                    }

                    sender.sendMessage(tl("banFailed", target.getDisplayName(), channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(tl("banDenied", target.getDisplayName(), channel.getColoredName()));
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
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
                    for (final IChatter member : channel.getMembers()) {
                        if (member.equals(sender) || !sender.canSee(member)) {
                            continue;
                        }

                        if (sender.canBan(member, channel)) {
                            if (StringUtil.startsWithIgnoreCase(member.getName(), input.get(TARGET))) {
                                completion.add(member.getName());
                            }
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (input.getLength() == CHANNEL + 1) {
            final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

            if (target == null || !sender.canSee(target)) {
                return Collections.emptyList();
            }

            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : target.getChannels()) {
                if (channel.isConversation() || channel.isDefault()) {
                    continue;
                }

                if (sender.canModerate(channel) && sender.canBan(target, channel)) {
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
