package de.g4memas0n.chats.event.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.configuration.ISettings;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Chatter Event that is called when a chatter send a message into a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
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
                                   @NotNull final String message) {
        super(sender, !Bukkit.isPrimaryThread());

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

    /**
     * Returns the used chat format for this chat message.
     *
     * @return the chat format
     * @see IChannel#getChatFormat()
     * @see ISettings#getChatFormat()
     */
    public @NotNull String getFormat() {
        return this.format;
    }

    /**
     * Sets the used chat format for this chat message.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} and {@link Placeholder#SENDER} placeholder,
     * otherwise it will throw an exception.</p>
     *
     * @param format the new chat format.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    public void setFormat(@NotNull final String format) throws IllegalArgumentException {
        if (!format.contains(Placeholder.SENDER.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {sender} placeholder", format));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.format = ChatColor.translateAlternateColorCodes('&', format);
    }

    /**
     * Returns the message of this chat message.
     *
     * @return the chat message.
     */
    public @NotNull String getMessage() {
        return this.message;
    }

    /**
     * Sets the message of this chat message.
     *
     * @param message the new chat message.
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
