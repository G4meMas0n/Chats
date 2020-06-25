package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid type.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidTypeException extends InputException {

    private final String identifier;

    public InvalidTypeException(@NotNull final String identifier) {
        super();

        this.identifier = identifier;
    }

    @SuppressWarnings("unused")
    public InvalidTypeException(@NotNull final String identifier,
                                @NotNull final Throwable cause) {
        super(cause);

        this.identifier = identifier;
    }

    @SuppressWarnings("unused")
    public InvalidTypeException(@NotNull final String key,
                                @NotNull final String identifier) {
        super(key);

        this.identifier = identifier;
    }

    @SuppressWarnings("unused")
    public InvalidTypeException(@NotNull final String key,
                                @NotNull final String identifier,
                                @NotNull final Throwable cause) {
        super(key, cause);

        this.identifier = identifier;
    }

    public @NotNull String getIdentifier() {
        return this.identifier;
    }
}
