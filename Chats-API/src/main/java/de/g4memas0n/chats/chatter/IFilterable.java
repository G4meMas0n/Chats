package de.g4memas0n.chats.chatter;

import org.jetbrains.annotations.NotNull;

/**
 * Filterable Interface that provides methods to filter chatters.
 *
 * <p>This Interface is only used for the {@link IChatter} Interface, to filter chatters.</p>
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IFilterable {

    /**
     * Returns whether the given chatter is in the range of this chatter.
     *
     * <p>When the given chatter is in an another World this method will return false.</p>
     *
     * @param chatter the chatter to check the range.
     * @param distance the max distance the chatter can be to be in range.
     * @return true when the given chatter is in range of this chatter,<br>
     *         false when it is not in range or it is not in the same world.
     */
    boolean isInRange(@NotNull final IChatter chatter, final int distance);

    /**
     * Returns whether the given chatter is in the world of this chatter.
     *
     * @param chatter the chatter to check the world.
     * @return true when the given chatter is in the world of this chatter, false otherwise.
     */
    boolean isInWorld(@NotNull final IChatter chatter);
}
