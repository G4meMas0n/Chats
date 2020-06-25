package de.g4memas0n.chats.listener;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import org.bukkit.event.Event;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.player.AsyncPlayerChatEvent;
import org.jetbrains.annotations.NotNull;

/**
 * The Player Listener, listening for the async player chat event.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 9th, 2020
 * changed: June 17th, 2020
 */
public final class PlayerListener extends BasicListener {

    /**
     * This method will perform all chat actions according the result of {@link Event#isAsynchronous()}.
     * When the given event is called asynchronous, the chat action will be performed asynchronous. When it is called
     * synchronous, the chat action will be synchronous scheduled on the next server tick.
     * @param event the called player chat event.
     */
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public void onPlayerChat(@NotNull final AsyncPlayerChatEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        event.setCancelled(true);

        final IChannel focus = chatter.getFocus();

        if (chatter.canSpeak(focus)) {
            if (focus.isMuted(chatter.getPlayer().getUniqueId())) {
                chatter.sendMessage(Messages.tl("mutedMember", focus.getColoredName()));
                return;
            }

            if (event.isAsynchronous()) {
                // Perform Chat Action from here, because this is already executed from an asynchronous thread.
                focus.performChat(chatter, event.getMessage());
            } else {
                this.getInstance().runSyncTask(() -> focus.performChat(chatter, event.getMessage()));
            }

            return;
        }

        chatter.sendMessage(Messages.tl("chatDenied", focus.getColoredName()));
    }
}
