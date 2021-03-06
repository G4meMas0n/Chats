package de.g4memas0n.chats.event.chatter;

import de.g4memas0n.chats.chatter.IChatter;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract Chatter Event.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class ChatterEvent extends Event {

    private final IChatter chatter;

    protected ChatterEvent(@NotNull final IChatter chatter) {
        this.chatter = chatter;
    }

    protected ChatterEvent(@NotNull final IChatter chatter, final boolean async) {
        super(async);

        this.chatter = chatter;
    }

    public final @NotNull IChatter getChatter() {
        return this.chatter;
    }
}
