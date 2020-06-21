package de.g4memas0n.chats.command;

import de.g4memas0n.chats.util.Permission;

/**
 * The Chats Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: March 5th, 2020
 * changed: June 20th, 2020
 */
public final class ChatsCommand extends BasicDelegateCommand {

    public ChatsCommand() {
        super("chats", 6, 1);

        this.addCommand(new CleanupCommand());
        this.addCommand(new HelpCommand());
        this.addCommand(new ReloadCommand());
        this.addCommand(new SocialSpyCommand());
        this.addCommand(new VersionCommand());

        this.setDescription("The main plugin command.");
        this.setPermission(Permission.USE.getNode());
        this.setUsage("/chats <command> [<arguments>]");
    }
}
