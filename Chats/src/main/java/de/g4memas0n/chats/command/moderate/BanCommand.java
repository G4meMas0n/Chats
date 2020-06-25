package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidPlayerException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The ban command that allows to ban a player from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: March 12th, 2020
 * changed: June 21th, 2020
 */
public final class BanCommand extends ModerateMemberCommand {

    public BanCommand() {
        super("ban", 2, 2);

        this.setDescription("Bans a player from a channel.");
        this.setPermission(Permission.BAN.getNode());
        this.setUsage("/channel ban <player> <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input,
                           @NotNull final IChannel channel) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (channel.isDefault()) {
                sender.sendMessage(Messages.tlErr("banDefault"));
                return true;
            }

            final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

            if (target == null || !sender.canSee(target)) {
                throw new InvalidPlayerException(input.get(TARGET));
            }

            if (target.equals(sender)) {
                sender.sendMessage(Messages.tlErr("banSelf"));
                return true;
            }

            if (channel.isBanned(target.getUniqueId())) {
                sender.sendMessage(Messages.tl("banAlready", target.getDisplayName(), channel.getColoredName()));
                return true;
            }

            if (!channel.isMember(target)) {
                sender.sendMessage(Messages.tl("noMember", target.getDisplayName(), channel.getColoredName()));
                return true;
            }

            if (sender.canBan(target, channel)) {
                if (channel.banMember(target)) {
                    sender.sendMessage(Messages.tl("banMember", target.getDisplayName(), channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("banFailed", target.getDisplayName(), channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("banDenied", target.getDisplayName(), channel.getColoredName()));
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

            if (target == null) {
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
