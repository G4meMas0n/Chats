package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Abstract input exception that indicates that a command input was invalid.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public abstract class InputException extends Exception {

    private final String key;

    protected InputException() {
        this.key = null;
    }

    protected InputException(@NotNull final Throwable cause) {
        super(cause);

        this.key = null;
    }

    protected InputException(@NotNull final String key) {
        this.key = key;
    }

    protected InputException(@NotNull final String key, @NotNull final Throwable cause) {
        super(cause);

        this.key = key;
    }

    public @Nullable String getKey() {
        return this.key;
    }
}
