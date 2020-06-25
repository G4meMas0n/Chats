package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid password.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidPasswordException extends InputException {

    private final String password;

    @SuppressWarnings("unused")
    public InvalidPasswordException(@NotNull final String password) {
        super();

        this.password = password;
    }

    public InvalidPasswordException(@NotNull final String password,
                                    @NotNull final Throwable cause) {
        super(cause);

        this.password = password;
    }

    @SuppressWarnings("unused")
    public InvalidPasswordException(@NotNull final String key,
                                    @NotNull final String password) {
        super(key);

        this.password = password;
    }

    @SuppressWarnings("unused")
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
