package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

public final class ChatterChangedChannelEvent extends ChatterEvent {
    private static final HandlerList handlers = new HandlerList();
    private final IChannel from;

    public ChatterChangedChannelEvent(@NotNull final IChatter chatter,
                                      @NotNull final IChannel from) {
        super(chatter, false);
        this.from = from;
    }

    @NotNull
    public IChannel getFrom() {
        return this.from;
    }

    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}