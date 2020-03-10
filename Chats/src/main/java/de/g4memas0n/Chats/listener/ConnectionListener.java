package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.jetbrains.annotations.NotNull;

/**
 * The Connection Listener, listening to player join and quit events, extends {@link BasicListener}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 7th, 2020
 * changed: March 10th, 2020
 */
public final class ConnectionListener extends BasicListener {

    public ConnectionListener() { }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerJoin(@NotNull final PlayerJoinEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().loadChatter(event.getPlayer());

        for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
            if (chatter.forcedJoin(current)) {
                chatter.joinChannel(current);
            }
        }

        for (final IChannel current : chatter.getChannels()) {
            if (chatter.forcedFocus(current)) {
                chatter.setFocus(current);
            }
        }

        chatter.getPlayer().sendMessage(Messages.tl("focusCurrent", chatter.getFocus().getColoredName()));
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerQuit(@NotNull final PlayerQuitEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        for (final IChannel current : chatter.getChannels()) {
            if (chatter.forcedLeave(current)) {
                chatter.leaveChannel(current);
            }
        }

        this.getInstance().getChatterManager().unloadChatter(chatter);
    }

    @Override
    protected @NotNull String[] getRegistered() {
        return new String[]{PlayerJoinEvent.class.getSimpleName(), PlayerQuitEvent.class.getSimpleName()};
    }
}
