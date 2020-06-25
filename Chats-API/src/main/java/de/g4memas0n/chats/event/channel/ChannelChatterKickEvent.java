package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Channel Event that is called when a chatter gets kicked from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: March 10th, 2020
 * changed: June 22th, 2020
 */
public final class ChannelChatterKickEvent extends ChannelEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IChatter chatter;
    private boolean cancel;

    public ChannelChatterKickEvent(@NotNull final IChannel channel,
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
