package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.Placeholder;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a channel performs a broadcast action.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: May 1st, 2020
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

    public @NotNull String getFormat() {
        return this.format;
    }

    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
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
