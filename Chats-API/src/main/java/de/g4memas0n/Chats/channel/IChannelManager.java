package de.g4memas0n.Chats.channel;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;

/**
 * Channel Manager Interface that defines a channel manager representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 4th, 2019
 * last change: November 15th, 2019
 */
public interface IChannelManager {

    /**
     * Returns the channel storage representation for this channel manager.
     * @return the channel storage of this channel manager.
     */
    @NotNull IChannelStorage getChannelStorage();

    /**
     * Returns the default channel of this channel manager.
     * @return the default channel.
     */
    @NotNull IChannel getDefaultChannel();

    /**
     * Sets the default channel of this channel manager and adds them to the listed channels if they currently not
     * contains they.
     * @param channel the new default channel for this channel manager.
     * @return true when the default channel was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel isn't a persist channel.
     */
    boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Sets the default channel of this channel manager.
     * Gets the channel with the given full name from this channel manager and sets them as default channel.
     * @param fullName the full name of the new default channel for this channel manager.
     * @return true when the default channel was changed as result of this call.
     * @throws IllegalArgumentException Thrown when no channel with the given full name was found in this channel
     *                                  manager or when the channel isn't a persist channel.
     */
    boolean setDefaultChannel(@NotNull final String fullName) throws IllegalArgumentException;

    /**
     * Returns if this channel manager contains a channel with the given full name.
     * @param fullName the full name of the channel that should be checked.
     * @return true when a channel with the given full name was found in this channel manager.
     */
    boolean hasChannel(@NotNull final String fullName);

    /**
     * Returns if this channel manager contains a persist channel with the given full name.
     * @param fullName the full name of the persist channel that should be checked.
     * @return true when a persist channel with the given full name was found in this channel manager.
     */
    boolean hasPersistChannel(@NotNull final String fullName);

    /**
     * Returns if this channel manager contains the given conversion channel.
     * @param fullName the full name of the conversion channel that should be checked.
     * @return true when the given conversion channel was found in this channel manager.
     */
    boolean hasConversionChannel(@NotNull final String fullName);

    /**
     * Returns a collection of all listed channels in this channel manager.
     * @return all channels of this channel manager.
     */
    @NotNull Collection<IChannel> getChannels();

    /**
     * Returns the channel with the given full channel name.
     * @param fullName the full name of the channel.
     * @return the channel with the given full name or null if there is no channel with the given full name.
     */
    @Nullable IChannel getChannel(@NotNull final String fullName);

    /**
     * Returns the conversion channel with the given full channel name if they exist.
     * @param fullName the full name of the conversion channel.
     * @return the conversion channel with the given full name or null if there is no conversion channel with the given
     *         full name or the founded channel is no conversion channel.
     */
    @Nullable IChannel getConversionChannel(@NotNull final String fullName);

    /**
     * Adds a new channel to this channel manager.
     * @param channel the channel that should be added.
     * @return true when the channel was added as result of this call.
     */
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes the channel with the given full name from this channel manager if the channel exists.
     * @param fullName the full name of the channel that should be removed.
     * @return true when the channel was removed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is the default channel of this channel manager.
     */
    boolean removeChannel(@NotNull final String fullName) throws IllegalArgumentException;

    /**
     * Updates the registered channel with the old full name to the new full name.
     * @param oldName the old full name of the channel.
     * @param newName the new full name of the channel.
     * @return true when the registered channel with the old full name was updated to the new full name.
     */
    boolean updateName(@NotNull final String oldName, @NotNull final String newName);
}
