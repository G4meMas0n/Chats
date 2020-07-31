package de.g4memas0n.chats.command;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that the channel, the user has specified, not exists.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public final class ChannelNotExistException extends InvalidArgumentException {

    public ChannelNotExistException(@NotNull final String channel) {
        super("channelNotExist", channel);
    }

    public ChannelNotExistException(@NotNull final Throwable cause,
                                    @NotNull final String channel) {
        super(cause, "channelNotExist", channel);
    }
}
