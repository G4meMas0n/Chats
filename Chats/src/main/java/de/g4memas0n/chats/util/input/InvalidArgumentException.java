package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that a argument, the user has specified, is invalid.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public class InvalidArgumentException extends InputException {

    private final String key;
    private final Object[] arguments;

    public InvalidArgumentException(@NotNull final String key,
                                    @NotNull final Object... arguments) {
        super();

        this.key = key;
        this.arguments = arguments;
    }

    public InvalidArgumentException(@NotNull final Throwable cause,
                                    @NotNull final String key,
                                    @NotNull final Object... arguments) {
        super(cause);

        this.key = key;
        this.arguments = arguments;
    }

    public final @NotNull String getKey() {
        return this.key;
    }

    public final @NotNull Object[] getArguments() {
        return this.arguments;
    }
}
