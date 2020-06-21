package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Channel Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 10th, 2020
 * changed: June 15th, 2020
 */
public final class ChannelCommand extends BasicDelegateCommand {

    public ChannelCommand() {
        super("channel", 18, 1);

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
        this.addCommand(new ModeratorCommand());
        this.addCommand(new ModifyCommand());
        this.addCommand(new MuteCommand());
        this.addCommand(new PardonCommand());
        this.addCommand(new UnmuteCommand());
        this.addCommand(new WhoCommand());

        this.setAliases(Collections.singletonList("ch"));
        this.setDescription("Manages the channels.");
        this.setPermission(Permission.CHANNEL.getNode());
        this.setUsage("/channel <command> [<arguments>]");
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == DELEGATE + 1) {
            // Tab complete for <command> argument is overridden to dynamically show/hide commands.
            // This means, for example that the modify command is only shown in tab-complete when the sender
            // can modify at least one existing channel.
            final List<String> completion = new ArrayList<>();

            for (final BasicCommand command : this.getCommands()) {
                if (!this.canTabComplete(sender, command)) {
                    continue;
                }

                if (StringUtil.startsWithIgnoreCase(command.getName(), arguments[DELEGATE])) {
                    completion.add(command.getName());
                }

                for (final String current : command.getAliases()) {
                    if (StringUtil.startsWithIgnoreCase(current, arguments[DELEGATE])) {
                        completion.add(current);
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return super.tabComplete(sender, alias, arguments);
    }

    private boolean canTabComplete(@NotNull final ICommandSource sender,
                                   @NotNull final BasicCommand command) {
        if (!sender.hasPermission(command.getPermission())) {
            return false;
        }

        // Show moderating commands only if sender can moderate at least one existing channel:
        if (command instanceof ModerateCommand) {
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

        // Show info command only if sender can view the information's of at least one existing channel:
        if (command instanceof InfoCommand) {
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

        // Show who command only if sender can view the members of at least one existing channel:
        if (command instanceof WhoCommand) {
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

        // Show modify command only if sender can modify at least one existing channel:
        if (command instanceof ModifyCommand) {
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

        // Show moderator command only if sender can modify the moderators in at least one existing channel:
        if (command instanceof ModeratorCommand) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModify(channel, ModifyType.MODERATORS)) {
                    return true;
                }
            }

            return false;
        }

        // Show delete command only if sender can delete at least one existing channel:
        if (command instanceof DeleteCommand) {
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
