package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.configuration.ISettings;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a channel performs a announce action.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChannelAnnounceEvent extends ChannelEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private boolean cancel;
    private String format;
    private String message;

    public ChannelAnnounceEvent(@NotNull final IChannel channel,
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
     * Returns the used announce format for this announce.
     *
     * @return the announce format
     * @see IChannel#getAnnounceFormat()
     * @see ISettings#getAnnounceFormat()
     */
    public @NotNull String getFormat() {
        return this.format;
    }

    /**
     * Sets the used announce format for this announce.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an
     * exception.</p>
     *
     * @param format the new announce format.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.format = format;
    }

    /**
     * Returns the message of this announce.
     *
     * @return the announce message.
     */
    public @NotNull String getMessage() {
        return this.message;
    }

    /**
     * Sets the message of this announce.
     *
     * @param message the new announce message.
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
