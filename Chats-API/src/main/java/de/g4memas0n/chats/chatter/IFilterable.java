package de.g4memas0n.chats.chatter;

import org.jetbrains.annotations.NotNull;

/**
 * Filterable Interface that provides methods to filter chatters.
 * This Interface is only used for the {@link IChatter} Interface, to filter chatters.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 9th, 2020
 * changed: January 9th, 2020
 */
public interface IFilterable {

    /**
     * Returns whether the given chatter is in the range of this chatter.
     * When the given chatter is in an another World this method will return false.
     * @param chatter the chatter to check the range.
     * @param distance the max distance the chatter can be to be in range.
     * @return true when the given chatter is in range of this chatter,
     *         false when it is not in range or it is not in the same world.
     */
    boolean isInRange(@NotNull final IChatter chatter, final int distance);

    /**
     * Returns whether the given chatter is in the world of this chatter.
     * @param chatter the chatter to check the world.
     * @return true when the given chatter is in the world of this chatter, false otherwise.
     */
    boolean isInWorld(@NotNull final IChatter chatter);
}
