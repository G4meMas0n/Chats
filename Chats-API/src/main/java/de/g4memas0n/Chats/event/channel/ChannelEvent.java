package de.g4memas0n.Chats.event.channel;

import de.g4memas0n.Chats.channel.IChannel;
import org.bukkit.event.Event;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract Channel Event.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: January 2nd, 2020
 */
public abstract class ChannelEvent extends Event {
    private final IChannel channel;

    public ChannelEvent(@NotNull final IChannel channel) {
        super();
        this.channel = channel;
    }

    public final @NotNull IChannel getChannel() {
        return this.channel;
    }
}
