package de.g4memas0n.Chats.event.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.messaging.Placeholder;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter send a message into a channel.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 11th, 2019
 * changed: January 2nd, 2020
 */
public final class ChatterChatChannelEvent extends ChatterEvent implements Cancellable {

    private static final HandlerList handlers = new HandlerList();

    private final IChannel channel;
    private boolean cancel;
    private String format;
    private String message;

    public ChatterChatChannelEvent(@NotNull final IChatter sender,
                                   @NotNull final IChannel channel,
                                   @NotNull final String format,
                                   @NotNull final String message,
                                   final boolean async) {
        super(sender, async);

        this.channel = channel;
        this.cancel = false;
        this.format = format;
        this.message = message;
    }

    @Override
    public boolean isCancelled() {
        return this.cancel;
    }

    @Override
    public void setCancelled(boolean cancel) {
        this.cancel = cancel;
    }

    public @NotNull IChannel getChannel() {
        return this.channel;
    }

    public @NotNull String getFormat() {
        return this.format;
    }

    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.SENDER.toString()) || !format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Invalid Format! Format must include the sender and message placeholder");
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
