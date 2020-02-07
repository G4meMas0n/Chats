package de.g4memas0n.Chats.event.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter joined a channel.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 5th, 2020
 * changed: January 5th, 2020
 */
public final class ChatterChannelJoinedEvent extends ChatterEvent {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel joined;

    public ChatterChannelJoinedEvent(@NotNull final IChatter chatter,
                                     @NotNull final IChannel joined) {
        super(chatter, false);
        this.joined = joined;
    }

    public @NotNull IChannel getJoined() {
        return this.joined;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }
}
