package de.g4memas0n.Chats.events;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.bukkit.event.player.PlayerEvent;
import org.jetbrains.annotations.NotNull;

public class PlayerChatChannelEvent extends PlayerEvent implements Cancellable {
    private static final HandlerList handlers = new HandlerList();
    private final IChatter chatter;
    private final IChannel channel;
    private boolean cancel;
    private String message;

    public PlayerChatChannelEvent(@NotNull final IChatter chatter,
                                  @NotNull final IChannel channel,
                                  @NotNull final String message) {
        super(chatter.getPlayer());
        this.chatter = chatter;
        this.channel = channel;
        this.cancel = false;
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

    @NotNull
    public IChatter getChatter() {
        return this.chatter;
    }

    @NotNull
    public IChannel getChannel() {
        return this.channel;
    }

    @NotNull
    public String getMessage() {
        return this.message;
    }

    public void setMessage(@NotNull final String message) {
        this.message = message;
    }

    @Override
    public HandlerList getHandlers() {
        return handlers;
    }

    public static HandlerList getHandlerList() {
        return handlers;
    }
}