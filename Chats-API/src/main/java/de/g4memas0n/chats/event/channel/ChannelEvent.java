package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract Channel Event.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class ChannelEvent extends Event {

    private final IChannel channel;

    protected ChannelEvent(@NotNull final IChannel channel) {
        super();
        this.channel = channel;
    }

    public final @NotNull IChannel getChannel() {
        return this.channel;
    }
}
