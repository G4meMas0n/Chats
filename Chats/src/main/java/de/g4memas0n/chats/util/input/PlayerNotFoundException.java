package de.g4memas0n.chats.util.input;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that the player, the user has specified, was not found.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: July 3rd, 2020
 * changed: July 5th, 2020
 */
public class PlayerNotFoundException extends InputException {

    private final String player;

    public PlayerNotFoundException(@NotNull final String player) {
        this.player = player;
    }

    public PlayerNotFoundException(@NotNull final Throwable cause,
                                   @NotNull final String player) {
        super(cause);

        this.player = player;
    }

    public @NotNull String getPlayer() {
        return this.player;
    }
}
