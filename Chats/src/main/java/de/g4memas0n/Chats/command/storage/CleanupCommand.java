package de.g4memas0n.chats.command.storage;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * The cleanup command that cleans up old chatter storage files.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 17th, 2020
 * changed: June 22th, 2020
 */
public final class CleanupCommand extends StorageCommand {

    private static final int DAYS = 0;

    public CleanupCommand() {
        super("cleanup", 1, 1);

        this.setDescription("Cleans up old chatter storage files.");
        this.setPermission(Permission.CLEANUP.getNode());
        this.setUsage("/chats cleanup <days>");
    }

    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            // Calculates the minimum difference between current time and last played time to delete a chatter.
            // Note: 1 Day = 24 Hours * 60 Minutes * 60 Seconds * 1.000 Milliseconds = 86.400.000 Milliseconds
            final long difference = input.getUnsignedLong(DAYS) * 86400000L;
            final long currentTime = System.currentTimeMillis();

            final Set<IOfflineChatter> cleanup = this.getInstance().getChatterManager().getOfflineChatters();
            final Future<?> load = this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::load));

            try {
                load.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            for (final IOfflineChatter chatter : cleanup) {
                final long lastPlayed = chatter.getLastPlayed();

                if (lastPlayed < 0) {
                    continue;
                }

                if ((currentTime - lastPlayed) < difference) {
                    cleanup.remove(chatter);
                }
            }

            final Future<?> delete = this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::delete));

            try {
                delete.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            sender.sendMessage(Messages.tl("cleanupFinish", cleanup.size()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        return Collections.emptyList();
    }
}
