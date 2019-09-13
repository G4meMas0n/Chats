package de.g4memas0n.Chats.channels;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a feature or a method of a channel is not supported.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: September 13th, 2019
 */
public final class UnsupportedFeatureException extends Exception {

    public UnsupportedFeatureException(@NotNull final String msg) {
        super(msg);
    }

    public UnsupportedFeatureException(@NotNull final Throwable cause) {
        super(cause);
    }

    public UnsupportedFeatureException(@NotNull final String msg,
                                       @NotNull final Throwable cause) {
        super(msg, cause);
    }
}
