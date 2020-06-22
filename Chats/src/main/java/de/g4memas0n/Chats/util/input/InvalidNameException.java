package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidNameException extends InputException {

    private final String name;

    public InvalidNameException(@NotNull final String name) {
        super();

        this.name = name;
    }

    public InvalidNameException(@NotNull final String name,
                                @NotNull final Throwable cause) {
        super(cause);

        this.name = name;
    }

    public InvalidNameException(@NotNull final String key,
                                @NotNull final String name) {
        super(key);

        this.name = name;
    }

    public InvalidNameException(@NotNull final String key,
                                @NotNull final String name,
                                @NotNull final Throwable cause) {
        super(key, cause);

        this.name = name;
    }

    public @NotNull String getName() {
        return this.name;
    }
}
