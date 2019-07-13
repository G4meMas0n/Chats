package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

public final class ChatterChatConversionEvent extends ChatterEvent implements Cancellable {
    private static final HandlerList handlers = new HandlerList();
    private final IChatter partner;
    private String message;
    private boolean cancel;

    public ChatterChatConversionEvent(@NotNull final IChatter sender,
                                      @NotNull final IChatter partner,
                                      @NotNull final String message) {
        super(sender, false);
        this.partner = partner;
        this.message = message;
        this.cancel = false;
    }

    @Override
    public boolean isCancelled() {
        return this.cancel;
    }

    @Override
    public void setCancelled(final boolean cancel) {
        this.cancel = cancel;
    }

    @NotNull
    public IChatter getPartner() {
        return this.partner;
    }

    @NotNull
    public String getMessage() {
        return this.message;
    }

    public void setMessage(@NotNull final String message) {
        this.message = message;
    }

    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}