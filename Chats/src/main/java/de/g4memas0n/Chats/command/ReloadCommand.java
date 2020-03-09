package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.type.ReloadType;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Reload Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 13th, 2020
 * changed: March 3rd, 2020
 */
public final class ReloadCommand extends BasicCommand {

    private static final String NAME = "reload";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = 1;

    private static final int ARG_RELOAD_TYPE = 0;

    public ReloadCommand() {
        super(NAME, Permission.ADMIN_RELOAD.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);

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
                        } catch (IOException ex) {
                            sender.sendMessage(Messages.tl("reloadFailed"));
                            return true;
                        }
                        break;
                    case CHANNELS:
                        try {
                            this.getInstance().getChannelManager().reload();
                        } catch (IOException ex) {
                            sender.sendMessage(Messages.tl("reloadFailed"));
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

                sender.sendMessage(Messages.tl("reloadSuccess"));
                return true;
            }

            sender.sendMessage(Messages.tl("reloadDenied"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (arguments.length == this.getMaxArgs()) {
                final List<String> completion = new ArrayList<>();
                final IPermissible permissible = this.getPermissible(sender);

                for (final ReloadType current : ReloadType.values()) {
                    if (InputUtil.containsInput(current.getIdentifier(), arguments[ARG_RELOAD_TYPE])) {
                        if (permissible.canReload(current)) {
                            completion.add(current.getIdentifier());
                        }
                    }
                }

                Collections.sort(completion);

                return completion;
            }
        }

        return Collections.emptyList();
    }
}
