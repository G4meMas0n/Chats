package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

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

    @NotNull
    public final IChatter getChatter() {
        return this.chatter;
    }
}