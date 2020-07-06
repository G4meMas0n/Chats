package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that the channel, the user has specified, not exists.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public class ChannelNotExistException extends InputException {

    private final String channel;

    public ChannelNotExistException(@NotNull final String channel) {
        this.channel = channel;
    }

    public ChannelNotExistException(@NotNull final Throwable cause,
                                    @NotNull final String channel) {
        super(cause);

        this.channel = channel;
    }

    public @NotNull String getChannel() {
        return this.channel;
    }
}
