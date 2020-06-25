package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a user input was a invalid player.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class InvalidPlayerException extends InputException {

    private final String name;

    public InvalidPlayerException(@NotNull final String name) {
        super();

        this.name = name;
    }

    public InvalidPlayerException(@NotNull final String name,
                                  @NotNull final Throwable cause) {
        super(cause);

        this.name = name;
    }

    public InvalidPlayerException(@NotNull final String key,
                                  @NotNull final String name) {
        super(key);

        this.name = name;
    }

    public InvalidPlayerException(@NotNull final String key,
                                  @NotNull final String name,
                                  @NotNull final Throwable cause) {
        super(key, cause);

        this.name = name;
    }

    public @NotNull String getName() {
        return this.name;
    }
}
