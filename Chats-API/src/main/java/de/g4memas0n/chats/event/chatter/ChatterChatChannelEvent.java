package de.g4memas0n.chats.event.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Placeholder;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter send a message into a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: July 11th, 2019
 * changed: May 1st, 2020
 */
public final class ChatterChatChannelEvent extends ChatterEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel channel;
    private String format;
    private String message;
    private boolean cancel;

    public ChatterChatChannelEvent(@NotNull final IChatter sender,
                                   @NotNull final IChannel channel,
                                   @NotNull final String format,
                                   @NotNull final String message,
                                   final boolean async) {
        super(sender, async);

        this.channel = channel;
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

    public @NotNull IChannel getChannel() {
        return this.channel;
    }

    public @NotNull String getFormat() {
        return this.format;
    }

    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.SENDER.toString()) && !format.contains(Placeholder.SENDER_PLAIN.toString())) {
            throw new IllegalArgumentException("Format is missing {sender} or {sender-plain} placeholder: " + format);
        }

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
