package de.g4memas0n.chats.command.manage.storage;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.stream.Collectors;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The reset command that resets chatter storage files.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ResetCommand extends BasicCommand {

    private static final int TARGET = 0;

    public ResetCommand() {
        super("reset", 1, 1);

        this.setPermission(Permission.RESET.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(TARGET));

            if (target == null) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            final IChatter online = target instanceof IChatter ? (IChatter) target : null;

            try {
                this.getInstance().runStorageTask(target::delete).get();

                if (online != null) {
                    this.getInstance().runStorageTask(online::load).get();
                    this.getInstance().runSyncTask(() -> online.sendMessage(tl("focusChannel", online.getFocus().getColoredName())));
                }

                sender.sendMessage(tl("resetChatter", online != null && sender.canSee(online) ? online.getDisplayName() : target.getName()));
                return true;
            } catch (ExecutionException ex) {
                this.getInstance().getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                this.getInstance().getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            sender.sendMessage(tl("resetFailed", online != null && sender.canSee(online) ? online.getDisplayName() : target.getName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TARGET + 1) {
            return this.getInstance().getChatterManager().getOfflineChatters().stream()
                    .map(IOfflineChatter::getName).sorted().collect(Collectors.toList());
        }

        return Collections.emptyList();
    }
}
