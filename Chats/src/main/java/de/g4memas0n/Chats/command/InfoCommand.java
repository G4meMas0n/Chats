package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.command.CommandSender;
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
 * changed: March 5th, 2020
 */
public final class InfoCommand extends BasicCommand {

    private static final String NAME = "info";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 1;

    private static final int ARG_CHANNEL = 0;

    public InfoCommand() {
        super(NAME, Permission.CHANNEL_INFO.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null) {
                sender.sendMessage(Messages.tlErr("channelNotExist"));
                return true;
            }

            if (permissible.canView(channel)) {
                sender.sendMessage(Messages.tl("infoHeader", channel.getColoredName()));

                sender.sendMessage(Messages.tl("infoFullName", channel.getFullName()));
                sender.sendMessage(Messages.tl("infoShortName", channel.getShortName()));
                sender.sendMessage(Messages.tl("infoColor",
                        channel.getChatColor() + channel.getChatColor().name()));

                sender.sendMessage(Messages.tl("infoPassword", channel.getPassword() != null ?
                        channel.getPassword() : ""));
                sender.sendMessage(Messages.tl("infoCrossWorld", channel.isCrossWorld() ?
                        Messages.tl("enabled") : Messages.tl("disabled")));
                sender.sendMessage(Messages.tl("infoDistance", channel.getDistance()));

                sender.sendMessage(Messages.tl("infoType", channel.getType().name()));

                String format = channel.getAnnounceFormat();
                boolean equalsDefault = format.equals(this.getInstance().getSettings().getAnnounceFormat());

                sender.sendMessage(Messages.tl("infoAnnounceFormat", format, Messages.tlBool(equalsDefault)));

                format = channel.getBroadcastFormat();
                equalsDefault = format.equals(this.getInstance().getSettings().getBroadcastFormat());

                sender.sendMessage(Messages.tl("infoBroadcastFormat", format, Messages.tlBool(equalsDefault)));

                format = channel.getChatFormat();
                equalsDefault = format.equals(this.getInstance().getSettings().getChatFormat());

                sender.sendMessage(Messages.tl("infoChatFormat", format, Messages.tlBool(equalsDefault)));

                sender.sendMessage(Messages.tl("infoCustomFormat", channel.isCustomFormat() ?
                        Messages.tl("enabled") : Messages.tl("disabled")));

                return true;
            }

            sender.sendMessage(Messages.tl("infoDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final List<String> completion = new ArrayList<>();
            final IPermissible permissible = this.getPermissible(sender);

            for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                if (current.isConversation()) {
                    continue;
                }

                if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                    if (permissible.canView(current)) {
                        completion.add(current.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
