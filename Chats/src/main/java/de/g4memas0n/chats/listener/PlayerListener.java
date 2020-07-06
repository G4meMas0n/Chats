package de.g4memas0n.chats.listener;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
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
 */
public final class PlayerListener extends BasicListener {

    /**
     * This method will perform all chat actions according the result of {@link Event#isAsynchronous()}.
     *
     * <p>When the given event is called asynchronous, the chat action will be performed asynchronous.<br/>
     * When it is called synchronous, the chat action will be synchronous scheduled on the next server tick.</p>
     *
     * @param event the called player chat event.
     */
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public void onPlayerChat(@NotNull final AsyncPlayerChatEvent event) {
        final IChatter chatter = this.getInstance().getChatterManager().getChatter(event.getPlayer());

        event.setCancelled(true);

        final IChannel focus = chatter.getFocus();

        if (chatter.canSpeak(focus)) {
            if (focus.isMuted(chatter.getUniqueId()) && !chatter.hasPermission(Permission.MUTE.getChildren("bypass"))) {
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
