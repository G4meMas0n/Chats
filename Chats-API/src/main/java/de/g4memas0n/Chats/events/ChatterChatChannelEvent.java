package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

public final class ChatterChatChannelEvent extends ChatterEvent implements Cancellable {
    private static final HandlerList handlers = new HandlerList();
    private final IChannel channel;
    private String message;
    private String format;
    private boolean cancel;

    public ChatterChatChannelEvent(@NotNull final IChatter sender,
                                   @NotNull final IChannel channel,
                                   @NotNull final String message,
                                   @NotNull final String format) {
        super(sender, false);
        this.channel = channel;
        this.message = message;
        this.format = format;
        this.cancel = false;
    }

    @Override
    public boolean isCancelled() {
        return this.cancel;
    }

    @Override
    public void setCancelled(boolean cancel) {
        this.cancel = cancel;
    }

    @NotNull
    public IChannel getChannel() {
        return this.channel;
    }

    @NotNull
    public String getMessage() {
        return this.message;
    }

    public void setMessage(@NotNull final String message) {
        this.message = message;
    }

    @NotNull
    public String getFormat() {
        return this.format;
    }

    public void setFormat(@NotNull final String format) {
        this.format = format;
    }

    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}