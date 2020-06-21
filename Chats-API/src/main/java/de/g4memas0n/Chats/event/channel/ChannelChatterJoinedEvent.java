package de.g4memas0n.chats.event.channel;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

/**
 * Channel Event that is called when a chatter joined a channel.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 10th, 2020
 * changed: March 10th, 2020
 */
public class ChannelChatterJoinedEvent extends ChannelEvent {

    private static final HandlerList handlers = new HandlerList();

    private final IChatter chatter;

    public ChannelChatterJoinedEvent(@NotNull final IChannel channel, @NotNull final IChatter chatter) {
        super(channel);

        this.chatter = chatter;
    }

    public @NotNull IChatter getChatter() {
        return this.chatter;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }
}
