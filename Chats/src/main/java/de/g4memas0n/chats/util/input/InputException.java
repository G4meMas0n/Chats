package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Abstract input exception that indicates that a command input was invalid.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class InputException extends Exception {

    protected InputException() {
        super();
    }

    protected InputException(@NotNull final Throwable cause) {
        super(cause);
    }
}
