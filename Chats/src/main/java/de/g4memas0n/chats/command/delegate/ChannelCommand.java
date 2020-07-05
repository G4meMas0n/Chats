package de.g4memas0n.chats.command.delegate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.chatter.ChatCommand;
import de.g4memas0n.chats.command.chatter.ChatterCommand;
import de.g4memas0n.chats.command.chatter.FocusCommand;
import de.g4memas0n.chats.command.chatter.JoinCommand;
import de.g4memas0n.chats.command.chatter.LeaveCommand;
import de.g4memas0n.chats.command.chatter.UnignoreCommand;
import de.g4memas0n.chats.command.info.ListCommand;
import de.g4memas0n.chats.command.manage.CreateCommand;
import de.g4memas0n.chats.command.manage.DeleteCommand;
import de.g4memas0n.chats.command.moderate.BanCommand;
import de.g4memas0n.chats.command.moderate.BroadcastCommand;
import de.g4memas0n.chats.command.moderate.KickCommand;
import de.g4memas0n.chats.command.moderate.ModerateCommand;
import de.g4memas0n.chats.command.moderate.MuteCommand;
import de.g4memas0n.chats.command.moderate.PardonCommand;
import de.g4memas0n.chats.command.moderate.UnmuteCommand;
import de.g4memas0n.chats.command.modify.ModifyCommand;
import de.g4memas0n.chats.command.view.InfoCommand;
import de.g4memas0n.chats.command.view.WhoCommand;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The channel command that bundles commands handling channels together.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 10th, 2020
 * changed: July 5th, 2020
 */
public final class ChannelCommand extends DelegateCommand {

    public ChannelCommand() {
        super("channel", 17);

        this.addCommand(new BanCommand());
        this.addCommand(new BroadcastCommand());
        this.addCommand(new ChatCommand());
        this.addCommand(new CreateCommand());
        this.addCommand(new DeleteCommand());
        this.addCommand(new FocusCommand());
        this.addCommand(new InfoCommand());
        this.addCommand(new JoinCommand());
        this.addCommand(new KickCommand());
        this.addCommand(new LeaveCommand());
        this.addCommand(new ListCommand());
        this.addCommand(new ModifyCommand());
        this.addCommand(new MuteCommand());
        this.addCommand(new PardonCommand());
        this.addCommand(new UnmuteCommand());
        this.addCommand(new UnignoreCommand());
        this.addCommand(new WhoCommand());

        this.setAliases(Collections.singletonList("ch"));
        this.setDescription("Manages the channels.");
        this.setPermission(Permission.CHANNEL.getNode());
        this.setUsage("/channel <command> [<arguments>]");
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == DELEGATE + 1) {
            // Tab complete for <command> argument is overridden to dynamically show/hide commands.
            // This means, for example that the modify command is only shown in tab-complete when the sender
            // can modify at least one existing channel.
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand delegate : this.getCommands()) {
                if (delegate instanceof ChatterCommand && !(sender instanceof IChatter)) {
                    continue;
                }

                if (sender.hasPermission(delegate.getPermission()) && this.canTabComplete(sender, delegate)) {
                    if (StringUtil.startsWithIgnoreCase(delegate.getName(), input.get(DELEGATE))) {
                        completion.add(delegate.getName());
                    }

                    for (final String alias : delegate.getAliases()) {
                        if (StringUtil.startsWithIgnoreCase(alias, input.get(DELEGATE))) {
                            completion.add(alias);
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return super.tabComplete(sender, input);
    }

    private boolean canTabComplete(@NotNull final ICommandSource sender,
                                   @NotNull final BasicCommand delegate) {
        // Show who command only if sender can view the members of at least one existing channel:
        if (delegate instanceof WhoCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewWho(channel)) {
                    return true;
                }
            }

            return false;
        }

        // Show moderating commands only if sender can moderate at least one existing channel:
        if (delegate instanceof ModerateCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
                    return true;
                }
            }

            return false;
        }

        // Show modify command only if sender can modify at least one existing channel:
        if (delegate instanceof ModifyCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModify(channel)) {
                    return true;
                }
            }

            return false;
        }

        // Show info command only if sender can view the information's of at least one existing channel:
        if (delegate instanceof InfoCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canViewInfo(channel)) {
                    return true;
                }
            }

            return false;
        }

        // Show delete command only if sender can delete at least one existing channel:
        if (delegate instanceof DeleteCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canDelete(channel)) {
                    return true;
                }
            }

            return false;
        }

        // Command can not be dynamically shown/hidden. So show it always.
        return true;
    }
}
