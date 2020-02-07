package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.ReloadType;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Reload Command, extends {@link ChatsCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 13th, 2020
 * changed: February 4th, 2020
 */
public final class ReloadCommand extends ChatsCommand {

    private static final String NAME = "reload";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = 1;

    private static final int ARG_RELOAD_TYPE = 0;

    public ReloadCommand() {
        super(NAME, Permission.ADMIN_RELOAD.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] arguments) {
        if (!sender.hasPermission(this.getPermission())) {
            sender.sendMessage(""); //TODO: Add localized 'command_permissionMessage' message.
            return true;
        }

        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getInstance().getChatterManager().getPermissible(sender);

            ReloadType reloadType = ReloadType.getDefault();

            if (arguments.length == this.getMaxArgs()) {
                reloadType = ReloadType.getType(arguments[ARG_RELOAD_TYPE]);

                if (reloadType == null) {
                    return false;
                }
            }

            if (permissible.canReload(reloadType)) {
                switch (reloadType) {
                    case ALL:
                        try {
                            this.getInstance().reloadConfig();
                            this.getInstance().getChannelManager().reload();
                            this.getInstance().getChatterManager().reload();
                        } catch (IOException ex) {
                            sender.sendMessage(""); //TODO: Add localized 'command_reloadFailed' message.
                            return true;
                        }
                        break;
                    case CHANNELS:
                        try {
                            this.getInstance().getChannelManager().reload();
                            this.getInstance().getChatterManager().reload();
                        } catch (IOException ex) {
                            sender.sendMessage(""); //TODO: Add localized 'command_reloadFailed' message.
                            return true;
                        }
                        break;
                    case CHATTERS:
                        this.getInstance().getChatterManager().reload();
                        break;
                    case CONFIG:
                        this.getInstance().reloadConfig();
                        break;
                }

                sender.sendMessage(""); //TODO: Add localized 'command_reloadSuccess' message.
                return true;
            }

            sender.sendMessage(""); //TODO: Add localized 'command_reloadDenied' message.
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                               @NotNull final Command command,
                                               @NotNull final String alias,
                                               @NotNull final String[] arguments) {
        final List<String> completion = new ArrayList<>();

        if (!sender.hasPermission(this.getPermission())) {
            return completion;
        }

        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getInstance().getChatterManager().getPermissible(sender);

            if (arguments.length == this.getMaxArgs()) {
                for (final ReloadType current : ReloadType.values()) {
                    if (permissible.canReload(current)) {
                        if (current.getIdentifier().contains(arguments[ARG_RELOAD_TYPE])) {
                            completion.add(current.getIdentifier());
                        }
                    }
                }

                Collections.sort(completion);
            }
        }

        return completion;
    }
}
