package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

public abstract class ChatterEvent extends Event {
    protected IChatter sender;

    ChatterEvent(@NotNull final IChatter sender, final boolean async) {
        super(async);
        this.sender = sender;
    }

    @NotNull
    public final IChatter getSender() {
        return this.sender;
    }
}