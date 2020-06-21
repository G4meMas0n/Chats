package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Ignore Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: June 19th, 2020
 */
public final class IgnoreCommand extends BasicCommand {

    private static final String UNIGNORE = "unignore";

    private static final int TARGET = 0;

    public IgnoreCommand() {
        super("ignore", 0, 1);

        this.setAliases(Collections.singletonList(UNIGNORE));
        this.setDescription("Ignores or unignores a player.");
        this.setPermission(Permission.IGNORE.getNode());
        this.setUsage("/ignore|unignore [<player>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter chatter = (IChatter) sender;

            if (arguments.length == this.getMaxArgs()) {
                if (alias.equalsIgnoreCase(UNIGNORE)) {
                    if (!chatter.isIgnore()) {
                        sender.sendMessage(Messages.tl("ignoreNobody"));
                        return true;
                    }

                    final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(arguments[TARGET]);

                    if (target == null) {
                        sender.sendMessage(Messages.tlErr("playerNotFound", arguments[TARGET]));
                        return true;
                    }

                    if (chatter.removeIgnore(target.getUniqueId())) {
                        sender.sendMessage(Messages.tl("unignoreChatter", target.getName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("unignoreAlready", target.getName()));
                    return true;
                }

                final IChatter target = this.getInstance().getChatterManager().getChatter(arguments[TARGET]);

                if (target == null || !sender.canSee(target)) {
                    sender.sendMessage(Messages.tlErr("playerNotFound", arguments[TARGET]));
                    return true;
                }

                if (target.equals(sender)) {
                    sender.sendMessage(Messages.tlErr("ignoreSelf"));
                    return true;
                }

                if (chatter.isIgnore(target.getUniqueId())) {
                    sender.sendMessage(Messages.tl("ignoreAlready", target.getDisplayName()));
                    return true;
                }

                if (sender.canIgnore(target)) {
                    if (chatter.addIgnore(target.getUniqueId())) {
                        sender.sendMessage(Messages.tl("ignoreChatter", target.getDisplayName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("ignoreAlready", target.getDisplayName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("ignoreDenied", target.getDisplayName()));
                return true;
            }

            // No target is specified. List all ignored chatters:
            if (chatter.isIgnore()) {
                final List<String> ignores = new ArrayList<>();

                for (final UUID current : chatter.getIgnores()) {
                    final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(current);

                    if (target != null) {
                        ignores.add(target.getName());
                    }
                }

                if (!ignores.isEmpty()) {
                    Collections.sort(ignores);

                    sender.sendMessage(Messages.tlJoin("ignoreList", ignores));
                    return true;
                }
            }

            sender.sendMessage(Messages.tl("ignoreListEmpty"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == TARGET + 1) {
            if (!(sender instanceof IChatter)) {
                return Collections.emptyList();
            }

            final IChatter chatter = (IChatter) sender;
            final List<String> completion = new ArrayList<>();

            if (alias.equalsIgnoreCase(UNIGNORE)) {
                for (final UUID current : chatter.getIgnores()) {
                    final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(current);

                    if (target == null) {
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(target.getName(), arguments[TARGET])) {
                        completion.add(target.getName());
                    }
                }
            } else {
                for (final IChatter current : this.getInstance().getChatterManager().getChatters()) {
                    if (!sender.canSee(current) || sender.equals(current)) {
                        continue;
                    }

                    if (sender.canIgnore(current)) {
                        if (StringUtil.startsWithIgnoreCase(current.getName(), arguments[TARGET])) {
                            completion.add(current.getName());
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
