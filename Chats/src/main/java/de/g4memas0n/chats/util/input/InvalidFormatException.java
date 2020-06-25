package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid format.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidFormatException extends InputException {

    private final String format;

    @SuppressWarnings("unused")
    public InvalidFormatException(@NotNull final String format) {
        super();

        this.format = format;
    }

    public InvalidFormatException(@NotNull final String format,
                                  @NotNull final Throwable cause) {
        super(cause);

        this.format = format;
    }

    @SuppressWarnings("unused")
    public InvalidFormatException(@NotNull final String key,
                                  @NotNull final String format) {
        super(key);

        this.format = format;
    }

    @SuppressWarnings("unused")
    public InvalidFormatException(@NotNull final String key,
                                  @NotNull final String format,
                                  @NotNull final Throwable cause) {
        super(key, cause);

        this.format = format;
    }

    public @NotNull String getFormat() {
        return this.format;
    }
}
