package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid chat color.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidColorException extends InputException {

    @SuppressWarnings("unused")
    public InvalidColorException() {
        super();
    }

    public InvalidColorException(@NotNull final Throwable cause) {
        super(cause);
    }

    @SuppressWarnings("unused")
    public InvalidColorException(@NotNull final String key) {
        super(key);
    }

    @SuppressWarnings("unused")
    public InvalidColorException(@NotNull final String key,
                                 @NotNull final Throwable cause) {
        super(key, cause);
    }
}
