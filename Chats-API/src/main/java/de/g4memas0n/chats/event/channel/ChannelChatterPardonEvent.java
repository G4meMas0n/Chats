package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Channel Event that is called when a chatter gets pardoned from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 21th, 2020
 * changed: June 22th, 2020
 */
public final class ChannelChatterPardonEvent extends ChannelEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IOfflineChatter chatter;
    private boolean cancel;

    public ChannelChatterPardonEvent(@NotNull final IChannel channel,
                                     @NotNull final IOfflineChatter chatter) {
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

    public @NotNull IOfflineChatter getChatter() {
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
