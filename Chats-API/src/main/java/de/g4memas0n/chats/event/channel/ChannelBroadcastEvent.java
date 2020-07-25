package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.configuration.ISettings;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a channel performs a broadcast action.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChannelBroadcastEvent extends ChannelEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private boolean cancel;
    private String format;
    private String message;

    public ChannelBroadcastEvent(@NotNull final IChannel channel,
                                 @NotNull final String format,
                                 @NotNull final String message) {
        super(channel);

        this.cancel = false;
        this.format = format;
        this.message = message;
    }

    @Override
    public boolean isCancelled() {
        return this.cancel;
    }

    @Override
    public void setCancelled(final boolean cancel) {
        this.cancel = cancel;
    }

    /**
     * Returns the used broadcast format for this broadcast.
     *
     * @return the announce format
     * @see IChannel#getBroadcastFormat()
     * @see ISettings#getBroadcastFormat()
     */
    public @NotNull String getFormat() {
        return this.format;
    }

    /**
     * Sets the used broadcast format for this broadcast.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an
     * exception.</p>
     *
     * @param format the new broadcast format.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.format = format;
    }

    /**
     * Returns the message of this broadcast.
     *
     * @return the broadcast message.
     */
    public @NotNull String getMessage() {
        return this.message;
    }

    /**
     * Sets the message of this broadcast.
     *
     * @param message the new broadcast message.
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
