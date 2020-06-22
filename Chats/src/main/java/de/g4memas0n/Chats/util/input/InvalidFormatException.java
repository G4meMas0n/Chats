package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidFormatException extends InputException {

    private final String format;

    public InvalidFormatException(@NotNull final String format) {
        super();

        this.format = format;
    }

    public InvalidFormatException(@NotNull final String format,
                                  @NotNull final Throwable cause) {
        super(cause);

        this.format = format;
    }

    public InvalidFormatException(@NotNull final String key,
                                  @NotNull final String format) {
        super(key);

        this.format = format;
    }

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
