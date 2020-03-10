package de.g4memas0n.Chats.event.channel;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Channel Event that is called when a chatter gets muted in a channel.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 10th, 2020
 * changed: March 10th, 2020
 */
public class ChannelChatterMuteEvent extends ChannelEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IChatter chatter;
    private boolean cancel;

    public ChannelChatterMuteEvent(@NotNull final IChannel channel,
                                   @NotNull final IChatter chatter) {
        super(channel);

        this.chatter = chatter;
        this.cancel = false;
    }

    @Override
    public boolean isCancelled() {
        return this.cancel;
    }

    @Override
    public void setCancelled(final boolean cancel) {
        this.cancel = cancel;
    }

    public @NotNull IChatter getChatter() {
        return this.chatter;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }
}
