package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidBooleanException extends InputException {

    public InvalidBooleanException() {
        super();
    }

    public InvalidBooleanException(@NotNull final Throwable cause) {
        super(cause);
    }

    public InvalidBooleanException(@NotNull final String key) {
        super(key);
    }

    public InvalidBooleanException(@NotNull final String key,
                                   @NotNull final Throwable cause) {
        super(key, cause);
    }
}
