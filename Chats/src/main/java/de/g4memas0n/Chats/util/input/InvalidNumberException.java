package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidNumberException extends InputException {

    public InvalidNumberException() {
        super();
    }

    public InvalidNumberException(@NotNull final Throwable cause) {
        super(cause);
    }

    public InvalidNumberException(@NotNull final String key) {
        super(key);
    }

    public InvalidNumberException(@NotNull final String key,
                                  @NotNull final Throwable cause) {
        super(key, cause);
    }
}
