package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.BlockCommandSender;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Ignore Command TabExecutor, extends {@link ChatsPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: February 3rd, 2020
 */
public final class IgnoreCommand extends ChatsPluginCommand {

    private static final String NAME = "ignore";
    private static final String UNIGNORE = "unignore";
    private static final int MIN_ARGS = 1;
    private static final int MAX_ARGS = 1;

    private static final int ARG_TARGET = 0;

    public IgnoreCommand() {
        super(NAME, Permission.CHATTER_IGNORE.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] arguments) {
        if (sender instanceof BlockCommandSender || sender instanceof ConsoleCommandSender) {
            sender.sendMessage(""); //TODO: Add localized 'command_illegalAccess' message.
            return true;
        }

        if (!sender.hasPermission(this.getPermission())) {
            sender.sendMessage(""); //TODO: Add localized 'command_permissionMessage' message.
            return true;
        }

        if (this.argsInRange(arguments.length)) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            if (chatter.getPlayer().getName().equals(arguments[ARG_TARGET])) {
                sender.sendMessage(""); //TODO: Add localized 'chatter_ignoreSelf' message.
                return true;
            }

            if (alias.equals(UNIGNORE)) {
                @SuppressWarnings("deprecation")
                final OfflinePlayer target = this.getInstance().getServer().getOfflinePlayer(arguments[ARG_TARGET]);

                if (chatter.isIgnoring()) {
                    if (chatter.removeIgnores(target.getUniqueId())) {
                        sender.sendMessage(""); //TODO: Add localized 'chatter_unignoreChatter' message.
                        return true;
                    }

                    sender.sendMessage(""); //TODO: Add localized 'chatter_unignoreAlready' message.
                    return true;
                }

                sender.sendMessage(""); //TODO: Add localized 'chatter_ignoreNobody' message.
                return true;
            }

            final Player target = this.getInstance().getServer().getPlayer(arguments[ARG_TARGET]);

            if (target == null || !chatter.getPlayer().canSee(target)) {
                sender.sendMessage(""); //TODO: Add localized 'chatter_ignoreNoPlayer' message.
                return true;
            }

            if (chatter.isIgnoring(target.getUniqueId())) {
                if (chatter.removeIgnores(target.getUniqueId())) {
                    sender.sendMessage(""); //TODO: Add localized 'chatter_unignoreChatter' message.
                }

                return true;
            }

            if (chatter.canIgnore(target)) {
                if (chatter.addIgnores(target.getUniqueId())) {
                    sender.sendMessage(""); //TODO: Add localized 'chatter_ignoreChatter' message.
                }

                return true;
            }

            sender.sendMessage(""); //TODO: Add localized 'chatter_ignoreDenied' message.
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

        if (sender instanceof BlockCommandSender || sender instanceof ConsoleCommandSender) {
            return completion;
        }

        if (!sender.hasPermission(this.getPermission())) {
            return completion;
        }

        if (this.argsInRange(arguments.length)) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            if (arguments.length == this.getMinArgs()) {
                if (alias.equals(UNIGNORE)) {
                    for (final UUID current : chatter.getIgnores()) {
                        final String name = this.getInstance().getServer().getOfflinePlayer(current).getName();

                        if (name != null && name.contains(arguments[ARG_TARGET])) {
                            completion.add(name);
                        }
                    }

                    Collections.sort(completion);
                    return completion;
                }

                for (final Player current : this.getInstance().getServer().getOnlinePlayers()) {
                    if (current.equals(chatter.getPlayer())) {
                        continue;
                    }

                    if (chatter.isIgnoring(current.getUniqueId()) || chatter.canIgnore(current)) {
                        if (current.getName().contains(arguments[ARG_TARGET])) {
                            completion.add(current.getName());
                        }
                    }
                }

                Collections.sort(completion);
            }
        }

        return completion;
    }
}
