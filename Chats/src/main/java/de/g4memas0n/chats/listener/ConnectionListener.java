package de.g4memas0n.chats.listener;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.jetbrains.annotations.NotNull;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * The Connection Listener, listening for player join and quit events.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ConnectionListener extends BasicListener {

    private final Map<UUID, Future<?>> loads;

    public ConnectionListener() {
        this.loads = new HashMap<>();
    }

    @EventHandler(priority = EventPriority.LOWEST)
    public void onPlayerJoinLoad(@NotNull final PlayerJoinEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().loadChatter(event.getPlayer());

        this.loads.put(chatter.getUniqueId(), this.getInstance().runStorageTask(chatter::load));
    }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPlayerJoinForce(@NotNull final PlayerJoinEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        try {
            final Future<?> task = this.loads.remove(chatter.getUniqueId());

            if (task == null || chatter.hasPermission(Permission.FORCE.getChildren("exempt"))) {
                return;
            }

            task.get();
        } catch (ExecutionException ex) {
            this.getInstance().getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
        } catch (InterruptedException ex) {
            this.getInstance().getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
            if (chatter.hasChannel(channel)) {
                continue;
            }

            if (chatter.forcedJoin(channel)) {
                chatter.joinChannel(channel, true);
            }
        }

        for (final IChannel channel : chatter.getChannels()) {
            if (chatter.forcedFocus(channel)) {
                chatter.setFocus(channel);
            }
        }
    }

    @EventHandler(priority = EventPriority.MONITOR)
    public void onPlayerJoinInform(@NotNull final PlayerJoinEvent event) {
        if (this.getInstance().getSettings().isInform()) {
            final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

            this.getInstance().scheduleSyncTask(() -> chatter.sendMessage(
                    Messages.tl("focusCurrent", chatter.getFocus().getColoredName())),
                    this.getInstance().getSettings().getInformDelay());
        }
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerQuitForce(@NotNull final PlayerQuitEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        if (chatter.hasPermission(Permission.FORCE.getChildren("exempt"))) {
            return;
        }

        for (final IChannel channel : chatter.getChannels()) {
            if (chatter.forcedLeave(channel)) {
                chatter.leaveChannel(channel, true);
            }
        }
    }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPlayerQuitUnload(@NotNull final PlayerQuitEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().unloadChatter(event.getPlayer());

        this.getInstance().runSyncTask(chatter::save);
    }
}
