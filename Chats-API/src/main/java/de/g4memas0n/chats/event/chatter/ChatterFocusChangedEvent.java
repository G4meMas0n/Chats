package de.g4memas0n.chats.event.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter changed the active channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChatterFocusChangedEvent extends ChatterEvent {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel from;

    public ChatterFocusChangedEvent(@NotNull final IChatter chatter,
                                    @NotNull final IChannel from) {
        super(chatter);

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
