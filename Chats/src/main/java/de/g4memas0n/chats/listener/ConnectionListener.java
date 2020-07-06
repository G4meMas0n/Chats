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

/**
 * The Connection Listener, listening for player join and quit events.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ConnectionListener extends BasicListener {

    public ConnectionListener() { }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerJoin(@NotNull final PlayerJoinEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().loadChatter(event.getPlayer());

        this.getInstance().runStorageTask(() -> delayedLoad(chatter));
    }

    private void delayedLoad(@NotNull final IChatter chatter) {
        chatter.load();

        this.getInstance().runSyncTask(() -> delayedJoin(chatter));
    }

    private void delayedJoin(@NotNull final IChatter chatter) {
        if (!chatter.hasPermission(Permission.FORCE.getChildren("exempt"))) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (chatter.forcedJoin(channel)) {
                    chatter.joinChannel(channel);
                }
            }

            for (final IChannel channel : chatter.getChannels()) {
                if (chatter.forcedFocus(channel)) {
                    chatter.setFocus(channel);
                }
            }
        }

        chatter.sendMessage(Messages.tl("focusCurrent", chatter.getFocus().getColoredName()));
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerQuit(@NotNull final PlayerQuitEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().unloadChatter(event.getPlayer());

        if (!chatter.hasPermission(Permission.FORCE.getChildren("exempt"))) {
            for (final IChannel channel : chatter.getChannels()) {
                if (chatter.forcedLeave(channel)) {
                    chatter.leaveChannel(channel);
                }
            }
        }

        this.getInstance().runStorageTask(chatter::save);
    }
}
