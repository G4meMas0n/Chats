package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidColorException extends InputException {

    public InvalidColorException() {
        super();
    }

    public InvalidColorException(@NotNull final Throwable cause) {
        super(cause);
    }

    public InvalidColorException(@NotNull final String key) {
        super(key);
    }

    public InvalidColorException(@NotNull final String key,
                                 @NotNull final Throwable cause) {
        super(key, cause);
    }
}
