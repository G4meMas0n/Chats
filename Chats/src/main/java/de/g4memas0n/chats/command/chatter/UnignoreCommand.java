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
 * The unignore command that allows to unignore a player.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class UnignoreCommand extends ChatterCommand {

    private static final int TARGET = 0;

    public UnignoreCommand() {
        super("unignore", 0 , 1);

        this.setDescription("Unignores a player or lists all ignored players.");
        this.setPermission(Permission.IGNORE.getNode());
        this.setUsage("/unignore [<player>]");
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

            if (!sender.isIgnore()) {
                sender.sendMessage(Messages.tl("ignoreNobody"));
                return true;
            }

            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(TARGET));

            if (target == null) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            final IChatter online = target instanceof IChatter ? (IChatter) target : null;

            if (sender.removeIgnore(target.getUniqueId())) {
                sender.sendMessage(Messages.tl("unignoreChatter", online != null && sender.canSee(online)
                        ? online.getDisplayName() : target.getName()));
                return true;
            }

            sender.sendMessage(Messages.tl("unignoreAlready", online != null && sender.canSee(online)
                    ? online.getDisplayName() : target.getName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TARGET + 1) {
            final List<String> completion = new ArrayList<>();

            for (final UUID uniqueId : sender.getIgnores()) {
                final IOfflineChatter ignored = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                if (ignored == null) {
                    sender.removeIgnore(uniqueId);
                    continue;
                }

                if (StringUtil.startsWithIgnoreCase(ignored.getName(), input.get(TARGET))) {
                    completion.add(ignored.getName());
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
