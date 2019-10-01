package de.g4memas0n.Chats.event;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter change the active channel.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 5th, 2019
 * last change: September 13th, 2019
 */
public final class ChatterChangedChannelEvent extends ChatterEvent {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel from;

    public ChatterChangedChannelEvent(@NotNull final IChatter chatter,
                                      @NotNull final IChannel from) {
        super(chatter, false);
        this.from = from;
    }

    public @NotNull IChannel getFrom() {
        return this.from;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }
}
