package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Ignore Command, extends {@link BasicPluginCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: March 3rd, 2020
 */
public final class IgnoreCommand extends BasicPluginCommand {

    private static final String NAME = "ignore";
    private static final int MIN_ARGS = 0;
    private static final int MAX_ARGS = 1;

    private static final String UN_IGNORE = "unignore";

    private static final int ARG_TARGET = 0;

    public IgnoreCommand() {
        super(NAME, Permission.CHATTER_IGNORE.getName(), MIN_ARGS, MAX_ARGS, Collections.singletonList(UN_IGNORE));
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof Player)) {
                return false;
            }

            final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

            if (arguments.length == this.getMinArgs()) {
                final List<String> ignores = new ArrayList<>();

                for (final UUID current : chatter.getIgnores()) {
                    final String name = this.getInstance().getServer().getOfflinePlayer(current).getName();

                    if (name != null && !name.isEmpty()) {
                        ignores.add(name);
                    }
                }

                Collections.sort(ignores);

                sender.sendMessage(Messages.tl("ignoreList",
                        String.join(Messages.tl("listDelimiter"), ignores)));
                return true;
            }

            if (chatter.getPlayer().getName().equalsIgnoreCase(arguments[ARG_TARGET])) {
                sender.sendMessage(Messages.tlErr("ignoreSelf"));
                return true;
            }

            if (alias.equalsIgnoreCase(UN_IGNORE)) {
                @SuppressWarnings("deprecation")
                final OfflinePlayer target = this.getInstance().getServer().getOfflinePlayer(arguments[ARG_TARGET]);

                if (chatter.isIgnoring()) {
                    if (chatter.removeIgnores(target.getUniqueId())) {
                        sender.sendMessage(Messages.tl("unIgnoreChatter", arguments[ARG_TARGET]));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("unIgnoreAlready", arguments[ARG_TARGET]));
                    return true;
                }

                sender.sendMessage(Messages.tl("ignoreNobody"));
                return true;
            }

            final Player target = this.getInstance().getServer().getPlayer(arguments[ARG_TARGET]);

            if (target == null || !chatter.getPlayer().canSee(target)) {
                sender.sendMessage(Messages.tlErr("playerNotFound"));
                return true;
            }

            if (chatter.isIgnoring(target.getUniqueId())) {
                if (chatter.removeIgnores(target.getUniqueId())) {
                    sender.sendMessage(Messages.tl("unIgnoreChatter", target.getName()));
                }

                return true;
            }

            if (chatter.canIgnore(target)) {
                if (chatter.addIgnores(target.getUniqueId())) {
                    sender.sendMessage(Messages.tl("ignoreChatter", target.getName()));
                }

                return true;
            }

            sender.sendMessage(Messages.tl("ignoreDenied", target.getName()));
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
                if (!(sender instanceof Player)) {
                    return Collections.emptyList();
                }

                final List<String> completion = new ArrayList<>();
                final IChatter chatter = this.getInstance().getChatterManager().getChatter((Player) sender);

                for (final UUID current : chatter.getIgnores()) {
                    final String name = this.getInstance().getServer().getOfflinePlayer(current).getName();

                    if (name != null && InputUtil.containsInput(name, arguments[ARG_TARGET])) {
                        completion.add(name);
                    }
                }

                if (alias.equalsIgnoreCase(UN_IGNORE)) {
                    Collections.sort(completion);

                    return completion;
                }

                for (final Player current : this.getInstance().getServer().getOnlinePlayers()) {
                    if (completion.contains(current.getName()) || current.equals(chatter.getPlayer())) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getName(), arguments[ARG_TARGET])) {
                        if (chatter.canIgnore(current)) {
                            completion.add(current.getName());
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
