package de.g4memas0n.Chats.util;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;

/**
 * Implements a Runnable for running a chat action in the next sync delayed task, implements {@link Runnable}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: January 11th, 2020
 */
public final class ChatRunnable implements Runnable {

    private final IChannel channel;
    private final IChatter sender;
    private final String message;

    public ChatRunnable(@NotNull final IChannel channel,
                        @NotNull final IChatter sender,
                        @NotNull final String message) {
        this.channel = channel;
        this.sender = sender;
        this.message = message;
    }

    @Override
    public void run() {
        this.channel.performChat(this.sender, this.message);
    }
}
