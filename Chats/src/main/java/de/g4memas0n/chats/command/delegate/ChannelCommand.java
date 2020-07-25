package de.g4memas0n.chats.command.delegate;

import de.g4memas0n.chats.command.chatter.ChatCommand;
import de.g4memas0n.chats.command.chatter.FocusCommand;
import de.g4memas0n.chats.command.chatter.JoinCommand;
import de.g4memas0n.chats.command.chatter.LeaveCommand;
import de.g4memas0n.chats.command.info.ListCommand;
import de.g4memas0n.chats.command.manage.CreateCommand;
import de.g4memas0n.chats.command.manage.DeleteCommand;
import de.g4memas0n.chats.command.manage.moderate.BanCommand;
import de.g4memas0n.chats.command.manage.moderate.BroadcastCommand;
import de.g4memas0n.chats.command.manage.moderate.KickCommand;
import de.g4memas0n.chats.command.manage.moderate.MuteCommand;
import de.g4memas0n.chats.command.manage.moderate.PardonCommand;
import de.g4memas0n.chats.command.manage.moderate.UnmuteCommand;
import de.g4memas0n.chats.command.manage.modify.ModifyCommand;
import de.g4memas0n.chats.command.info.view.InfoCommand;
import de.g4memas0n.chats.command.info.view.WhoCommand;
import de.g4memas0n.chats.permission.Permission;
import java.util.Collections;

/**
 * The channel command that bundles commands handling channels together.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChannelCommand extends DelegateCommand {

    public ChannelCommand() {
        super("channel", 16);

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
        this.addCommand(new WhoCommand());

        this.setAliases(Collections.singletonList("ch"));
        this.setDescription("Manages the channels.");
        this.setPermission(Permission.CHANNEL.getNode());
        this.setUsage("/channel <command> [<arguments>]");
    }
}
