package de.g4memas0n.Chats.event.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter leaved a channel.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 5th, 2020
 * changed: January 5th, 2020
 */
public final class ChatterChannelLeavedEvent extends ChatterEvent {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel leaved;

    public ChatterChannelLeavedEvent(@NotNull final IChatter chatter,
                                     @NotNull final IChannel leaved) {
        super(chatter, false);
        this.leaved = leaved;
    }

    public @NotNull IChannel getJoined() {
        return this.leaved;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }
}
