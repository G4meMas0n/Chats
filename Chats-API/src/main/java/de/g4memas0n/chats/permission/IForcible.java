package de.g4memas0n.chats.permission;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;

/**
 * Forcible Interface that provides methods to check whether a forcible will be forced to something on login or logout.
 *
 * <p>This Interface is used for the {@link IChatter} Interface, to determine whether the chatter is forced to join,
 * focus and/or leave channels.</p>
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IForcible {

    /**
     * Returns whether this forcible is forced to focus the given channel on connect.
     *
     * @param channel the channel to check the forced focus.
     * @return true when this forcible is forced to focus the given channel, false otherwise.
     */
    boolean forcedFocus(@NotNull final IChannel channel);

    /**
     * Returns whether this forcible is forced to join the given channel on connect.
     *
     * @param channel the channel to check the forced join.
     * @return true when this forcible is forced to join the given channel, false otherwise.
     */
    boolean forcedJoin(@NotNull final IChannel channel);

    /**
     * Returns whether this forcible is forced to leave the given channel on connect and disconnect.
     *
     * @param channel the channel to check the forced leave.
     * @return true when this forcible is forced to leave the given channel, false otherwise.
     */
    boolean forcedLeave(@NotNull final IChannel channel);
}
