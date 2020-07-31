package de.g4memas0n.chats.command.info.view;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.channel.IChannel.Information;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
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

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlBool;
import static de.g4memas0n.chats.messaging.Messages.tlState;

/**
 * The info command that allows to show information's of a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class InfoCommand extends BasicCommand {

    private static final int CHANNEL = 0;

    public InfoCommand() {
        super("info", 1, 1);

        this.setPermission(Permission.VIEW_INFO.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewInfo(channel)) {
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

            if (sender.canViewInfo(channel)) {
                sender.sendMessage(tl("infoHeader", channel.getColoredName()));
                sender.sendMessage(tl("infoFullName", channel.getFullName()));

                if (sender.canView(channel, Information.SHORT_NAME)) {
                    sender.sendMessage(tl("infoShortName", channel.getShortName()));
                }

                if (sender.canView(channel, Information.COLOR)) {
                    sender.sendMessage(tl("infoColor", channel.getColor() + channel.getColor().name().toLowerCase()));
                }

                if (sender.canView(channel, Information.PASSWORD) && channel.getPassword() != null) {
                    sender.sendMessage(tl("infoPassword", channel.getPassword()));
                }

                if (sender.canView(channel, Information.CROSS_WORLD)) {
                    sender.sendMessage(tl("infoCrossWorld", tlState(channel.isCrossWorld())));
                }

                if (sender.canView(channel, Information.DISTANCE)) {
                    sender.sendMessage(tl("infoDistance", channel.getDistance()));
                }

                if (sender.canView(channel, Information.TYPE)) {
                    sender.sendMessage(tl("infoType", tl(channel.getType().getIdentifier())));
                }

                if (sender.canView(channel, Information.VERBOSE)) {
                    sender.sendMessage(tl("infoVerbose", tlState(channel.isVerbose())));
                }

                if (sender.canView(channel, Information.OWNER) && channel.getOwner() != null) {
                    final IOfflineChatter owner = this.getInstance().getChatterManager().getOfflineChatter(channel.getOwner());

                    sender.sendMessage(tl("infoOwner", owner != null ? owner.getName() : channel.getOwner()));
                }

                if (sender.canView(channel, Information.FORMATS)) {
                    sender.sendMessage(tl("infoCustomFormat", tlState(channel.isCustomFormat())));

                    String format = channel.getAnnounceFormat();
                    sender.sendMessage(tl("infoFormat", tl("announce"), format.replace("§", "&"),
                            tlBool(this.getInstance().getSettings().getAnnounceFormat().equals(format))));

                    format = channel.getBroadcastFormat();
                    sender.sendMessage(tl("infoFormat", tl("broadcast"), format.replace("§", "&"),
                            tlBool(this.getInstance().getSettings().getBroadcastFormat().equals(format))));

                    format = channel.getChatFormat();
                    sender.sendMessage(tl("infoFormat", tl("chat"), format.replace("§", "&"),
                            tlBool(this.getInstance().getSettings().getChatFormat().equals(format))));
                }

                return true;
            }

            sender.sendMessage(tl("infoDenied", channel.getColoredName()));
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

                if (sender.canViewInfo(channel)) {
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
