package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The ignore command that allows to ignore a player.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class IgnoreCommand extends ChatterCommand {

    private static final int TARGET = 0;

    public IgnoreCommand() {
        super("ignore", 0, 1);

        this.setDescription("Ignores a player or list all ignored players.");
        this.setPermission(Permission.IGNORE.getNode());
        this.setUsage("/ignore [<player>]");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMinArgs()) {
                final List<String> ignores = new ArrayList<>();

                for (final UUID uniqueId : sender.getIgnores()) {
                    final IOfflineChatter ignored = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (ignored == null) {
                        sender.removeIgnore(uniqueId);
                        continue;
                    }

                    ignores.add(ignored.getName());
                }

                if (ignores.isEmpty()) {
                    sender.sendMessage(Messages.tl("ignoreNobody"));
                    return true;
                }

                Collections.sort(ignores);

                sender.sendMessage(Messages.tlJoin("ignoreList", ignores));
                return true;
            }

            final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

            if (target == null || !sender.canSee(target)) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            if (target.equals(sender)) {
                sender.sendMessage(Messages.tlErr("ignoreSelf"));
                return true;
            }

            if (sender.isIgnore(target.getUniqueId())) {
                sender.sendMessage(Messages.tl("ignoreAlready", target.getDisplayName()));
                return true;
            }

            if (sender.canIgnore(target)) {
                if (sender.addIgnore(target.getUniqueId())) {
                    sender.sendMessage(Messages.tl("ignoreChatter", target.getDisplayName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("ignoreAlready", target.getDisplayName()));
                return true;
            }

            sender.sendMessage(Messages.tl("ignoreDenied", target.getDisplayName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TARGET + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                if (target.equals(sender) || sender.isIgnore(target.getUniqueId()) || !sender.canSee(target)) {
                    continue;
                }

                if (sender.canIgnore(target)) {
                    if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(TARGET))) {
                        completion.add(target.getName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
