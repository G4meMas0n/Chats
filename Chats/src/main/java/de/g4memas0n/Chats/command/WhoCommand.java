package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.InfoType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Who Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 17th, 2020
 * changed: June 20th, 2020
 */
public final class WhoCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public WhoCommand() {
        super("who", 1 ,1);

        this.setDescription("Shows the members of a channel.");
        this.setPermission(Permission.VIEW_WHO.getNode());
        this.setUsage("/channel who <channel>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canViewWho(channel)) {
                final List<String> members = new ArrayList<>();

                final boolean viewOwn = sender.canView(channel, InfoType.OWNER);
                final boolean viewMods = sender.canView(channel, InfoType.MODERATORS);
                final boolean viewMutes = sender.canView(channel, InfoType.MUTES);

                for (final IChatter member : channel.getMembers()) {
                    if (!sender.canSee(member)) {
                        continue;
                    }

                    final StringBuilder displayed = new StringBuilder();

                    if (channel.isOwner(member.getUniqueId()) && viewOwn) {
                        displayed.append(Messages.tl("prefixOwner"));
                    }

                    if (channel.isModerator(member.getUniqueId()) && viewMods) {
                        displayed.append(Messages.tl("prefixModerator"));
                    }

                    if (channel.isMuted(member.getUniqueId()) && viewMutes) {
                        displayed.append(Messages.tl("prefixMuted"));
                    }

                    displayed.append(member.getDisplayName());

                    members.add(displayed.toString());
                }

                if (!members.isEmpty()) {
                    Collections.sort(members);

                    sender.sendMessage(Messages.tl("whoHeader", channel.getColoredName()));
                    sender.sendMessage(Messages.tlJoin("whoList", members));
                    return true;
                }

                sender.sendMessage(Messages.tl("whoEmpty", channel.getColoredName()));
                return true;
            }

            sender.sendMessage(Messages.tl("whoDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewWho(channel)) {
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
