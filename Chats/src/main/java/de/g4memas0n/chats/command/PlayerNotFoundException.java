package de.g4memas0n.chats.command;

import org.jetbrains.annotations.NotNull;

/**
 * Thrown to indicate that the player, the user has specified, was not found.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public final class PlayerNotFoundException extends InputException {

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