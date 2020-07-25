package de.g4memas0n.chats.command.delegate;

import de.g4memas0n.chats.command.manage.storage.CleanupCommand;
import de.g4memas0n.chats.command.info.HelpCommand;
import de.g4memas0n.chats.command.manage.storage.ReloadCommand;
import de.g4memas0n.chats.command.manage.SocialSpyCommand;
import de.g4memas0n.chats.command.info.VersionCommand;
import de.g4memas0n.chats.command.manage.storage.SaveCommand;
import de.g4memas0n.chats.permission.Permission;

/**
 * The chats command that bundles commands handling the plugin together.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChatsCommand extends DelegateCommand {

    public ChatsCommand() {
        super("chats", 6);

        this.addCommand(new CleanupCommand());
        this.addCommand(new HelpCommand());
        this.addCommand(new ReloadCommand());
        this.addCommand(new SaveCommand());
        this.addCommand(new SocialSpyCommand());
        this.addCommand(new VersionCommand());

        this.setDescription("The main plugin command.");
        this.setPermission(Permission.USE.getNode());
        this.setUsage("/chats <command> [<arguments>]");
    }
}
