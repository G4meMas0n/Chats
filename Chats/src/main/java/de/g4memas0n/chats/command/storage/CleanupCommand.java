package de.g4memas0n.chats.command.storage;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.Iterator;
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
 * changed: July 5th, 2020
 */
public final class CleanupCommand extends BasicCommand {

    private static final int INFO_COUNT = 100;

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
            final long cleanupTime = System.currentTimeMillis();

            final Set<IOfflineChatter> cleanup = this.getInstance().getChatterManager().getOfflineChatters();

            if (cleanup.size() > INFO_COUNT) {
                sender.sendMessage(Messages.tl("cleanupRunning"));
            }

            final Future<?> load = this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::load));

            try {
                load.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);

                sender.sendMessage(Messages.tl("cleanupFailed"));
                return true;
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            Log.getPlugin().debug(String.format("Loaded %d offline chatters after approximately %d milliseconds.",
                    cleanup.size(), System.currentTimeMillis() - cleanupTime));

            for (final Iterator<IOfflineChatter> iterator = cleanup.iterator(); iterator.hasNext();) {
                final IOfflineChatter chatter = iterator.next();
                final long lastPlayed = chatter.getLastPlayed();

                if (lastPlayed < 0) {
                    continue;
                }

                if ((cleanupTime - lastPlayed) < difference) {
                    iterator.remove();
                }
            }

            final Future<?> delete = this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::delete));

            try {
                delete.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);

                sender.sendMessage(Messages.tl("cleanupFailed"));
                return true;
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            Log.getPlugin().debug(String.format("Finished cleanup after approximately %d milliseconds.",
                    System.currentTimeMillis() - cleanupTime));

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
