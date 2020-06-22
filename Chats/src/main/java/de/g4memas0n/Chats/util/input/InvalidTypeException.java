package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidTypeException extends InputException {

    private final String identifier;

    public InvalidTypeException(@NotNull final String identifier) {
        super();

        this.identifier = identifier;
    }

    public InvalidTypeException(@NotNull final String identifier,
                                @NotNull final Throwable cause) {
        super(cause);

        this.identifier = identifier;
    }

    public InvalidTypeException(@NotNull final String key,
                                @NotNull final String identifier) {
        super(key);

        this.identifier = identifier;
    }

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
