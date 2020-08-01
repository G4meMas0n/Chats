package de.g4memas0n.chats.command.manage.storage;

import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.storage.IStorageHolder;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The cleanup command that cleans up old chatter storage files.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class CleanupCommand extends BasicCommand {

    private static final String[] COMMON_DAYS = new String[]{"7", "30", "90", "180", "365"};

    private static final int DAYS = 0;

    public CleanupCommand() {
        super("cleanup", 1, 1);

        this.setPermission(Permission.CLEANUP.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            this.getInstance().getLogger().info("Starting cleanup of offline chatters. This may took a while.");

            // Calculates the minimum difference between current time and last played time to delete a chatter.
            // Note: 1 Day = 24 Hours * 60 Minutes * 60 Seconds * 1.000 Milliseconds = 86.400.000 Milliseconds
            final long difference = input.getUnsignedLong(DAYS) * 86400000L;
            final long cleanupTime = System.currentTimeMillis();

            final Set<IOfflineChatter> cleanup = this.getInstance().getChatterManager().getOfflineChatters();

            if (cleanup.isEmpty()) {
                this.getInstance().getLogger().info("Aborted cleanup. No storage files has been found.");

                sender.sendMessage(tl("cleanupNobody"));
                return true;
            }

            try {
                final long loadTime = System.currentTimeMillis();

                this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::load)).get();
                this.getInstance().getLogger().debug(String.format("Loaded %d offline chatters after approximately %d "
                        + "milliseconds.", cleanup.size(), System.currentTimeMillis() - loadTime));

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

                if (cleanup.isEmpty()) {
                    this.getInstance().getLogger().info("Aborted cleanup. No old storage files has been found");

                    sender.sendMessage(tl("cleanupNobody"));
                    return true;
                }

                final long deleteTime = System.currentTimeMillis();

                this.getInstance().runStorageTask(() -> cleanup.forEach(IStorageHolder::delete)).get();
                this.getInstance().getLogger().debug(String.format("Deleted %d offline chatters after approximately %d "
                        + "milliseconds.", cleanup.size(), System.currentTimeMillis() - deleteTime));
                this.getInstance().getLogger().info(String.format("Finished cleanup after approximately %d milliseconds.",
                        System.currentTimeMillis() - cleanupTime));

                sender.sendMessage(tl("cleanupFinish", cleanup.size()));
                return true;
            } catch (ExecutionException ex) {
                this.getInstance().getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                this.getInstance().getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            sender.sendMessage(tl("cleanupFailed"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == DAYS + 1) {
            final List<String> completion = new ArrayList<>();

            for (final String days : COMMON_DAYS) {
                if (StringUtil.startsWithIgnoreCase(days, input.get(DAYS))) {
                    completion.add(days);
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
