package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.InfoType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Info Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: March 5th, 2020
 * changed: June 20th, 2020
 */
public final class InfoCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public InfoCommand() {
        super("info", 1, 1);

        this.setDescription("Shows information's of a channel.");
        this.setPermission(Permission.VIEW_INFO.getNode());
        this.setUsage("/channel info <channel>");
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

            if (sender.canViewInfo(channel)) {
                sender.sendMessage(Messages.tl("infoHeader", channel.getColoredName()));
                sender.sendMessage(Messages.tl("infoFullName", channel.getFullName()));

                if (sender.canView(channel, InfoType.SHORT_NAME)) {
                    sender.sendMessage(Messages.tl("infoShortName", channel.getShortName()));
                }

                if (sender.canView(channel, InfoType.COLOR)) {
                    sender.sendMessage(Messages.tl("infoColor", channel.getColor() + channel.getColor().name().toLowerCase()));
                }

                if (channel.getPassword() != null && sender.canView(channel, InfoType.PASSWORD)) {
                    sender.sendMessage(Messages.tl("infoPassword", channel.getPassword()));
                }

                if (sender.canView(channel, InfoType.CROSS_WORLD)) {
                    sender.sendMessage(Messages.tl("infoCrossWorld", Messages.tlState(channel.isCrossWorld())));
                }

                if (sender.canView(channel, InfoType.DISTANCE)) {
                    sender.sendMessage(Messages.tl("infoDistance", channel.getDistance()));
                }

                if (sender.canView(channel, InfoType.TYPE)) {
                    sender.sendMessage(Messages.tl("infoType", Messages.tlType(channel.getType())));
                }

                if (channel.getOwner() != null && sender.canView(channel, InfoType.OWNER)) {
                    final IOfflineChatter owner = this.getInstance().getChatterManager().getOfflineChatter(channel.getOwner());

                    sender.sendMessage(Messages.tl("infoOwner", owner != null ? owner.getName() : channel.getOwner().toString()));
                }

                if (sender.canView(channel, InfoType.FORMATS)) {
                    sender.sendMessage(Messages.tl("infoCustomFormat", Messages.tlState(channel.isCustomFormat())));

                    String format = channel.getAnnounceFormat();
                    sender.sendMessage(Messages.tl("infoFormat", Messages.tl("announce"), format,
                            Messages.tlBool(this.getInstance().getSettings().getAnnounceFormat().equals(format))));

                    format = channel.getBroadcastFormat();
                    sender.sendMessage(Messages.tl("infoFormat", Messages.tl("broadcast"), format,
                            Messages.tlBool(this.getInstance().getSettings().getBroadcastFormat().equals(format))));

                    format = channel.getChatFormat();
                    sender.sendMessage(Messages.tl("infoFormat", Messages.tl("chat"), format,
                            Messages.tlBool(this.getInstance().getSettings().getChatFormat().equals(format))));
                }

                return true;
            }

            sender.sendMessage(Messages.tl("infoDenied", channel.getColoredName()));
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

                if (sender.canViewInfo(channel)) {
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
