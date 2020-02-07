package de.g4memas0n.Chats.event.chatter;

import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Placeholder;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter starts a conversion with an another chatter.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 12th, 2019
 * changed: January 17th, 2020
 */
public final class ChatterChatConversationEvent extends ChatterEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IChatter partner;
    private String format;
    private String message;
    private boolean cancel;

    public ChatterChatConversationEvent(@NotNull final IChatter sender,
                                        @NotNull final IChatter partner,
                                        @NotNull final String format,
                                        @NotNull final String message) {
        super(sender, false);

        this.partner = partner;
        this.format = format;
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

    public @NotNull IChatter getPartner() {
        return this.partner;
    }

    public @NotNull String getFormat() {
        return this.format;
    }

    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Invalid format! Missing " + Placeholder.MESSAGE + " Placeholder");
        }

        if (!format.contains(Placeholder.CON_PARTNER.toString())) {
            throw new IllegalArgumentException("Invalid format! Missing " + Placeholder.CON_PARTNER + " Placeholder");
        }

        if (!format.contains(Placeholder.CON_SENDER.toString())
                || !format.contains(Placeholder.CON_ADDRESS.toString())) {
            throw new IllegalArgumentException("Invalid format! Missing " + Placeholder.CON_SENDER + " or "
                    + Placeholder.CON_ADDRESS + " Placeholder");
        }

        this.format = format;
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
