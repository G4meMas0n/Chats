package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * The Cleanup Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.3-SNAPSHOT
 *
 * created: June 17th, 2020
 * changed: June 20th, 2020
 */
public final class CleanupCommand extends BasicCommand {

    private static final int DAYS = 0;

    public CleanupCommand() {
        super("cleanup", 1, 1);

        this.setDescription("Cleans up old chatter storage files.");
        this.setPermission(Permission.CLEANUP.getNode());
        this.setUsage("/chats cleanup <days>");
    }

    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            try {
                // Calculates the minimum difference between current time and last played time to delete a chatter.
                // Note: 1 Day = 24 Hours * 60 Minutes * 60 Seconds * 1.000 Milliseconds = 86.400.000 Milliseconds
                final long difference = Long.parseUnsignedLong(arguments[DAYS]) * 86400000L;
                final long currentTime = System.currentTimeMillis();

                sender.sendMessage(Messages.tl("cleanupRunning"));

                final Set<IOfflineChatter> cleanup = this.getInstance().getChatterManager().getOfflineChatters();
                final Future<?> load = this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::load));

                load.get();

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

                delete.get();

                sender.sendMessage(Messages.tl("cleanupFinish", cleanup.size()));
                return true;
            } catch (NumberFormatException ex) {
                sender.sendMessage(Messages.tlErr("invalidNumber"));
                return true;
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            sender.sendMessage(Messages.tlErr("cleanupFailed"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        return Collections.emptyList();
    }
}
