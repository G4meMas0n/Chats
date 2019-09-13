package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract Chatter Event.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 5th, 2019
 * last change: September 11th, 2019
 */
public abstract class ChatterEvent extends Event {
    private final IChatter chatter;

    public ChatterEvent(@NotNull final IChatter chatter) {
        super();
        this.chatter = chatter;
    }

    ChatterEvent(@NotNull final IChatter chatter, final boolean async) {
        super(async);
        this.chatter = chatter;
    }

    public final @NotNull IChatter getChatter() {
        return this.chatter;
    }
}
