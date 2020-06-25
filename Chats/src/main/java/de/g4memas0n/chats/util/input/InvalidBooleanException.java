package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid boolean value.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidBooleanException extends InputException {

    public InvalidBooleanException() {
        super();
    }

    @SuppressWarnings("unused")
    public InvalidBooleanException(@NotNull final Throwable cause) {
        super(cause);
    }

    public InvalidBooleanException(@NotNull final String key) {
        super(key);
    }

    @SuppressWarnings("unused")
    public InvalidBooleanException(@NotNull final String key,
                                   @NotNull final Throwable cause) {
        super(key, cause);
    }
}
