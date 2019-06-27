package de.g4memas0n.Chats.managers;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.storages.IChannelStorage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public interface IChannelManager {
    int MIN_CHATTER_COUNT = 2;
    int MAX_CHATTER_COUNT = 2;

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
     * @return true if the default channel was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel isn't a global channel.
     */
    boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Sets the default channel of this channel manager.
     * Gets the channel with the given full name from this channel manager and sets them as default channel.
     * @param fullName the full name of the new default channel for this channel manager.
     * @return true if the default channel was changed as result of this call.
     * @throws IllegalArgumentException Thrown when no channel with the given full name was found in this channel
     *                                  manager or when the channel isn't a global channel.
     */
    boolean setDefaultChannel(@NotNull final String fullName) throws IllegalArgumentException;

    /**
     * Returns if this channel manager contains the given channel.
     * @param channel the channel that should be checked.
     * @return true if the given channel was found in this channel manager.
     */
    boolean hasChannel(@NotNull final IChannel channel);

    /**
     * Returns if this channel manager contains a channel with the given full name.
     * @param fullName the full name of the channel that should be checked.
     * @return true if a channel with the given full name was found in this channel manager.
     */
    boolean hasChannel(@NotNull final String fullName);

    /**
     * Returns if this channel manager contains the given conversion channel.
     * @param channel the conversion channel that should be checked.
     * @return true if the given conversion channel was found in this channel manager.
     * @throws IllegalArgumentException Thrown when the given channel isn't a conversion channel.
     */
    boolean hasConversionChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Returns if this channel manager contains a conversion channel with the given full name.
     * @param fullName the full name of the conversion channel.
     * @return true if a conversion channel with the given full name was found in this channel manager.
     */
    boolean hasConversionChannel(@NotNull final String fullName);

    /**
     * Returns if this channel manager contains a conversion channel with the given chatters.
     * Builds the full name of the conversation channel with the given chatters and checks if this channel manager
     * contains a conversion channel with this full name.
     * @param chatters the chatters that are in the conversion channel.
     * @return true if a channel with the given chatters was found in this channel manager.
     */
    boolean hasConversionChannel(@NotNull final IChatter... chatters);

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
     * Returns the conversion channel with the given full channel name.
     * @param fullName the full name of the conversion channel.
     * @return the conversion channel with the given full name or null if there is no conversion channel with the given
     *         full name or the channel with the given full name isn't a conversion channel.
     */
    @Nullable IChannel getConversionChannel(@NotNull final String fullName);

    /**
     * Returns the conversion channel with the given full channel name.
     * Builds the full name of the conversion channel with the given chatters and returns the conversion channel with
     * the full name.
     * @param chatters the chatters that are in the conversion channel.
     * @return the conversion channel with the full name or null if there is no conversion channel with the full name
     *         or the channel with the full name isn't a conversion channel.
     */
    @Nullable IChannel getConversionChannel(@NotNull final IChatter... chatters);

    /**
     * Adds a new channel to this channel manager.
     * @param channel the channel that should be added.
     * @return true if the channel was added as result of this call.
     */
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes a channel from this channel manager.
     * Gets the full name of the specified channel and removes them from this channel manager.
     * @param channel the channel that should be removed.
     * @return true if the channel was removed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is the default channel of this channel manager.
     */
    boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Removes a channel from this channel manager.
     * @param fullName the full name of the channel that should be removed.
     * @return true if the channel was removed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is the default channel of this channel manager.
     */
    boolean removeChannel(@NotNull final String fullName) throws IllegalArgumentException;

    /**
     * Removes a conversion channel from this channel manager.
     * @param channel the conversion channel that should be removed.
     * @return true if the conversion channel was removed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel isn't a conversion channel.
     */
    boolean removeConversionChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Removes a conversion channel from this channel manager.
     * @param fullName the full name of the conversion channel that should be removed.
     * @return true if the conversion channel was removed as result of this call.
     */
    boolean removeConversionChannel(@NotNull final String fullName);

    /**
     * Removes a conversion channel from this channel manager.
     * Builds the full name of the conversion channel with the given chatters and removes them from this channel
     * manager.
     * @param chatters the chatters that are in the conversion channel that should be removed.
     * @return true if the conversion channel was removed as result of this call.
     */
    boolean removeConversionChannel(@NotNull final IChatter... chatters);

    /**
     * Builds the full name for a conversion channel with the given chatters they are in the conversion channel.
     * Gets the player UUID for all chatters, sorts them and put them together separated by an underscore.
     * @param chatters the chatters that are in the conversion channel.
     * @return the full name for the conversion channel.
     * @throws IllegalArgumentException Thrown when too few or too many chatters are given.
     */
    @NotNull
    static String buildConversionFullName(@NotNull final IChatter... chatters) throws IllegalArgumentException {
        if (chatters.length < MIN_CHATTER_COUNT || chatters.length > MAX_CHATTER_COUNT) {
            throw new IllegalArgumentException("too few or too many chatters for a conversion channel.");
        }

        ArrayList<String> playerUUIDs = new ArrayList<>();

        for (IChatter current : chatters) {
            playerUUIDs.add(current.getPlayer().getUniqueId().toString());
        }

        Collections.sort(playerUUIDs);

        return String.join("_", playerUUIDs);
    }

    /**
     * Builds the short name for a conversion channel with the given chatters they are in this conversion channel.
     * Gets the player name for all chatters, sorts them and put them together separated by an underscore.
     * @param chatters the chatters that are in the conversion channel.
     * @return the short name for the conversion channel.
     * @throws IllegalArgumentException Thrown when too few or too many chatters are given.
     */
    @NotNull
    static String buildConversionShortName(@NotNull final IChatter... chatters) throws IllegalArgumentException {
        if (chatters.length < MIN_CHATTER_COUNT || chatters.length > MAX_CHATTER_COUNT) {
            throw new IllegalArgumentException("too few or too many chatters for a conversion channel.");
        }

        ArrayList<String> playerNames = new ArrayList<>();

        for (IChatter current : chatters) {
            playerNames.add(current.getPlayer().getName());
        }

        Collections.sort(playerNames);

        return String.join("_", playerNames);
    }
}