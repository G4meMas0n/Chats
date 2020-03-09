package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.ChatRunnable;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.player.AsyncPlayerChatEvent;
import org.jetbrains.annotations.NotNull;

/**
 * The Player Listener, listening to async player chat event, extends {@link BasicListener}.
 * This listener schedules the chat performing to the next sync delayed task.
 * So all chat actions will performed synchronously.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 9th, 2020
 * changed: February 17th, 2020
 */
public final class PlayerListener extends BasicListener {

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public void onPlayerChat(@NotNull final AsyncPlayerChatEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        event.setCancelled(true);

        final IChannel focus = chatter.getFocus();

        if (chatter.canSpeak(focus)) {
            final Runnable runnable = new ChatRunnable(chatter.getFocus(), chatter, event.getMessage());

            this.getInstance().getServer().getScheduler().scheduleSyncDelayedTask(this.getInstance(), runnable);

            return;
        }

        event.getPlayer().sendMessage(Messages.tl("chatDenied", focus.getFullName()));
    }

    @Override
    protected @NotNull String[] getRegistered() {
        return new String[]{AsyncPlayerChatEvent.class.getSimpleName()};
    }
}
