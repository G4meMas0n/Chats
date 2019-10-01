package de.g4memas0n.Chats.event;

import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;
import java.util.Set;

/**
 * Chatter Event that is called when a chatter starts a conversion with an another chatter.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 12th, 2019
 * last change: September 11th, 2019
 */
public final class ChatterChatConversionEvent extends ChatterEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final Set<IChatter> partners;

    private String message;
    private boolean cancel;

    public ChatterChatConversionEvent(@NotNull final IChatter sender,
                                      @NotNull final Set<IChatter> partners,
                                      @NotNull final String message) {
        super(sender, false);
        this.partners = partners;
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

    public @NotNull Set<IChatter> getPartners() {
        return this.partners;
    }

    public @NotNull String getMessage() {
        return this.message;
    }

    public void setMessage(@NotNull final String message) {
        this.message = message;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public @NotNull static HandlerList getHandlerList() {
        return handlers;
    }
}
