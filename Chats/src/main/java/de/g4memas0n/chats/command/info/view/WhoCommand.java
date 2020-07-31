package de.g4memas0n.chats.command.info.view;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.channel.IChannel.Information;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlJoin;

/**
 * The who command that allows to show the members of a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class WhoCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public WhoCommand() {
        super("who", 1 ,1);

        this.setPermission(Permission.VIEW_WHO.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewWho(channel)) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canViewWho(channel)) {
                final List<String> members = new ArrayList<>();

                final boolean viewOwn = sender.canView(channel, Information.OWNER);
                final boolean viewMutes = sender.canView(channel, Information.MUTES);

                for (final IChatter member : channel.getMembers().stream().sorted().collect(Collectors.toList())) {
                    if (!sender.canSee(member)) {
                        continue;
                    }

                    final StringBuilder displayed = new StringBuilder();

                    if (channel.isOwner(member.getUniqueId()) && viewOwn) {
                        displayed.append(tl("prefixOwner"));
                    }

                    if (channel.isMuted(member.getUniqueId()) && viewMutes) {
                        displayed.append(tl("prefixMuted"));
                    }

                    members.add(displayed.append(member.getDisplayName()).toString());
                }

                if (members.isEmpty()) {
                    sender.sendMessage(tl("whoNobody", channel.getColoredName()));
                    return true;
                }

                sender.sendMessage(tl("whoHeader", channel.getColoredName()));
                sender.sendMessage(tlJoin("whoList", members));
                return true;
            }

            sender.sendMessage(tl("whoDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewWho(channel)) {
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
