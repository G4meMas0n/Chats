package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

public class InvalidPasswordException extends InputException {

    private final String password;

    public InvalidPasswordException(@NotNull final String password) {
        super();

        this.password = password;
    }

    public InvalidPasswordException(@NotNull final String password,
                                    @NotNull final Throwable cause) {
        super(cause);

        this.password = password;
    }

    public InvalidPasswordException(@NotNull final String key,
                                    @NotNull final String password) {
        super(key);

        this.password = password;
    }

    public InvalidPasswordException(@NotNull final String key,
                                    @NotNull final String password,
                                    @NotNull final Throwable cause) {
        super(key, cause);

        this.password = password;
    }

    public @NotNull String getPassword() {
        return this.password;
    }
}
