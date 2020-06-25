package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid number value.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidNumberException extends InputException {

    public InvalidNumberException() {
        super();
    }

    public InvalidNumberException(@NotNull final Throwable cause) {
        super(cause);
    }

    @SuppressWarnings("unused")
    public InvalidNumberException(@NotNull final String key) {
        super(key);
    }

    @SuppressWarnings("unused")
    public InvalidNumberException(@NotNull final String key,
                                  @NotNull final Throwable cause) {
        super(key, cause);
    }
}
