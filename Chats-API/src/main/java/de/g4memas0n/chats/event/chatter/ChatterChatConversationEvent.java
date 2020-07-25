package de.g4memas0n.chats.event.chatter;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.configuration.ISettings;
import org.bukkit.Bukkit;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter starts a conversion with an another chatter.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
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
        super(sender, !Bukkit.isPrimaryThread());

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

    /**
     * Returns the used conversation format for this conversation.
     *
     * @return the conversation format.
     * @see ISettings#getConversationFormat()
     */
    public @NotNull String getFormat() {
        return this.format;
    }

    /**
     * Sets the used conversation format for this conversation.
     *
     * <p>The given format must include the {@link Placeholder#CON_ADDRESS}, {@link Placeholder#CON_PARTNER} and
     * {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an exception.</p>
     *
     * @param format the new conversation format.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.CON_ADDRESS.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {con-address} placeholder", format));
        }

        if (!format.contains(Placeholder.CON_PARTNER.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {con-partner} placeholder", format));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        this.format = format;
    }

    /**
     * Returns the message of this conversation.
     *
     * @return the conversation message.
     */
    public @NotNull String getMessage() {
        return this.message;
    }

    /**
     * Sets the message of this conversation.
     *
     * @param message the new conversation message.
     */
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
