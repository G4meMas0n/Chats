package de.g4memas0n.chats;

import de.g4memas0n.chats.channel.IChannelManager;
import de.g4memas0n.chats.chatter.IChatterManager;
import de.g4memas0n.chats.messaging.IFormatter;
import de.g4memas0n.chats.storage.configuration.ISettings;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.plugin.Plugin;
import org.bukkit.scheduler.BukkitTask;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.concurrent.Future;

/**
 * Chats Interface, that defines the main class of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChats extends Plugin {

    /**
     * Returns the channel manager of this plugin, that handle all channels.
     *
     * @return the used channel manager.
     */
    @NotNull IChannelManager getChannelManager();

    /**
     * Returns the chatter manager of this plugin, that handle all chatters.
     *
     * @return the used chatter manager.
     */
    @NotNull IChatterManager getChatterManager();

    /**
     * Returns the formatter of this plugin, used for all chat formatting actions.
     *
     * @return the used formatter.
     */
    @NotNull IFormatter getFormatter();

    /**
     * Returns the settings of this plugin.
     *
     * @return the used settings.
     */
    @NotNull ISettings getSettings();

    /**
     * Returns the chat service of this plugin, used for extended chat formatting.
     *
     * <p>Can be null when there is no registered chat service of the vault plugin.</p>
     *
     * @return the used chat service.
     */
    @Nullable Chat getChatService();

    /**
     * Sets the chat service for this plugin.
     *
     * <p>The given service can be null to remove the chat service.</p>
     *
     * @param service the by vault registered chat service.
     */
    void setChatService(@Nullable final Chat service);

    /**
     * Submits a task for execution on the storage thread and returns a Future representing that task.
     *
     * <p>The Future's get method will return null upon successful completion.</p>
     *
     * @param task the task to submit.
     * @return a Future representing pending completion of the task.
     * @see Future#get()
     */
    @NotNull Future<?> runStorageTask(@NotNull final Runnable task);

    /**
     * Submits a task for execution on the next server tick on the main server thread and returns a BukkitTask
     * representing that task.
     *
     * @param task the task to run.
     * @return a BukkitTask that contains the id number.
     */
    @NotNull BukkitTask runSyncTask(@NotNull final Runnable task);

    /**
     * Schedules a task for execution after the delay specified in the settings on the storage thread and returns a
     * Future representing that task.
     *
     * <p>The Future's get method will return null upon successful completion.</p>
     *
     * @param task the task to schedule.
     * @return a Future representing pending completion of the task.
     * @see Future#get()
     */
    @NotNull Future<?> scheduleStorageTask(@NotNull final Runnable task);

    /**
     * Schedules a task for execution after the given ticks on the main server thread and returns a BukkitTask
     * representing that task.
     *
     * @param task the task to schedule.
     * @param delay the ticks to wait before running the task
     * @return a BukkitTask that contains the id number
     */
    @SuppressWarnings("unused")
    @NotNull BukkitTask scheduleSyncTask(@NotNull final Runnable task, final long delay);
}
