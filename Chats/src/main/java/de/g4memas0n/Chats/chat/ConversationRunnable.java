package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;

/**
 * Implements a Runnable for running a conversation action in the next sync delayed task, implements {@link Runnable}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 15th, 2020
 * changed: January 17th, 2020
 */
public final class ConversationRunnable implements Runnable {

    private final IChatPerformer performer;
    private final IChatter sender;
    private final IChatter partner;
    private final String message;

    public ConversationRunnable(@NotNull final IChatPerformer performer,
                                @NotNull final IChatter sender,
                                @NotNull final IChatter partner,
                                @NotNull final String message) {
        this.performer = performer;
        this.sender = sender;
        this.partner = partner;
        this.message = message;
    }

    @Override
    public void run() {
        this.performer.performConversion(this.sender, this.partner, this.message);
    }
}
