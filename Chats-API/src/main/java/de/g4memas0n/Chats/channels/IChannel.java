package de.g4memas0n.Chats.channels;

import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.formatters.IChannelFormatter;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;

/**
 * Channel Interface that defines a channel representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: September 13th, 2019
 */
public interface IChannel {

    // Methods for Channel Properties:
    /**
     * Returns the listed full name of this channel.
     * @return the full name of this channel.
     */
    @NotNull String getFullName();

    /**
     * Returns the listed short name of this channel.
     * @return the short name of this channel.
     */
    @NotNull String getShortName();

    /**
     * Sets a new short name for this channel.
     * Removes the short name when the given param is null.
     * @param shortName the new short name for this channel.
     * @return true when the short name was changed as result of this call.
     *         false when the short name was not changed or when this channel do not support this feature.
     */
    boolean setShortName(@Nullable final String shortName);

    /**
     * Returns the listed chat color of this channel.
     * @return the color of this channel.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets a new color for this channel.
     * @param chatColor the new chat color for this channel.
     * @return true when this channel color was changed as result of this call.
     *         false when the color was not changed or when this channel do not support this feature.
     */
    boolean setChatColor(@NotNull final ChatColor chatColor);

    // Methods for Channel Types:
    /**
     * Returns if this channel is a persist channel.
     * @return true when this channel is persist.
     */
    boolean isPersistChannel();

    /**
     * Returns the channel type of this channel.
     * @return true when this channel is a global channel.
     */
    boolean isGlobalChannel();

    /**
     * Returns the channel type of this channel.
     * @return true when this channel is a conversion channel.
     */
    boolean isConversionChannel();


    // Methods for Channel Settings:
    /**
     * Returns if this channel is worlds across.
     * @return true when this channel is world across.
     */
    boolean isCrossWorld();

    /**
     * Sets the across world option of this channel.
     * @param state if this channel should be across worlds.
     * @return true when the across world option was changed as result of this call.
     *         false when this option was not changed or when this channel do not support this feature.
     */
    boolean setCrossWorld(final boolean state);

    /**
     * Returns if this channel has a distance.
     * @return true when this channel has a distance.
     */
    boolean hasDistance();

    /**
     * Returns the distance of this channel.
     * @return the distance of this channel or -1 if this channel has no distance.
     */
    int getDistance();

    /**
     * Sets a new distance for this channel.
     * @param distance the new distance for this channel.
     * @return true when the distance was changed as result of this call.
     *         false when the option was not changed or when this channel do not support this feature.
     */
    boolean setDistance(final int distance);

    /**
     * Returns if this channel has set a password or not.
     * @return true when this channel has set a password and false if the channel has not set a password.
     */
    boolean hasPassword();

    /**
     * Returns the password of this channel.
     * @return the password of this channel or null when no password is set.
     */
    @Nullable String getPassword();

    /**
     * Sets a new password for this channel.
     * Removes the password when the given param is null.
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call.
     *         false when the password was not changed or when this channel do not support this feature.
     */
    boolean setPassword(@Nullable final String password);

    // Methods for Channel Formatting:
    /**
     * Returns the message formatter from this channel.
     * @return the channel formatter.
     */
    @NotNull IChannelFormatter getFormatter();

    /**
     * Returns if this channel use custom format or use the default format.
     * @return true when this channel use a custom format.
     */
    boolean isUseCustomFormat();

    /**
     * Sets the use custom format option for this channel.
     * @param state if this channel should use a custom format.
     * @return true when the use custom format option was changed as result of this call.
     *         false when the option was not changed or when this channel do not support this feature.
     */
    boolean setUseCustomFormat(final boolean state);

    /**
     * Returns the announce format from this channel.
     * @return the announce format or an empty string when this channel do not support this feature.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the announce format for this channel.
     * @param format the new announce format.
     * @return true when the announce format was changed as result of this call.
     *         false when the format was not changed or when this channel do not support this feature.
     */
    boolean setAnnounceFormat(@Nullable final String format);

    /**
     * Returns the broadcast format from this channel.
     * @return the broadcast format or an empty string when this channel do not support this feature.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the broadcast format for this channel.
     * @param format the new broadcast format.
     * @return true when the broadcast format was changed as result of this call.
     *         false when the format was not changed or when this channel do not support this feature.
     */
    boolean setBroadcastFormat(@Nullable final String format);

    /**
     * Returns the channel format from this channel.
     * @return the channel format.
     */
    @NotNull String getChannelFormat();

    /**
     * Sets the channel format for this channel.
     * @param format the new channel format.
     * @return true when the channel format was changed as result of this call.
     *         false when the format was not changed.
     */
    boolean setChannelFormat(@Nullable final String format);

    // Methods for Chatter Collection of this Channel:
    /**
     * Returns all listed chatters of this channel.
     * @return all chatters listed in this channel.
     */
    @NotNull Set<IChatter> getChatters();

    /**
     * Adds a new chatter to this channel.
     * This method only adds the given chatter to this channel and do not add this channel to the given chatter.
     * @param chatter a chatter who should be listed in this channel.
     * @return true when the collection was changed as result of this call.
     *         false when the collection was not changed or when this channel do not support this feature.
     */
    boolean addChatter(@NotNull final IChatter chatter);

    /**
     * Removes a chatter of this channel.
     * This method only removes the given chatter from this channel and do not remove this channel from
     * the given chatter.
     * @param chatter a chatter who should be removed from this channel.
     * @return true when the chatter was removed as result of this call.
     *         false when the collection was not changed or when this channel do not support this feature.
     */
    boolean removeChatter(@NotNull final IChatter chatter);

    // Method for performing Chat:
    /**
     * Perform the announce action for this channel.
     * Checks all conditions to perform the announce action successfully and then sends all listed player in this
     * channel the given message.
     * @param message the message for this announce.
     * @throws UnsupportedFeatureException Thrown when this channel do not support announces.
     */
    void performAnnounce(@NotNull final String message) throws UnsupportedFeatureException;

    /**
     * Perform the broadcast action for this channel.
     * Checks all conditions to perform the broadcast action successfully and then sends all listed player in this
     * channel the broadcast with the given message.
     * @param message the message for this broadcast.
     * @throws UnsupportedFeatureException Thrown when this channel do not support broadcasts.
     */
    void performBroadcast(@NotNull final String message) throws UnsupportedFeatureException;

    /**
     * Perform the chat action for this channel.
     * Checks all conditions to perform the chat action successfully and then sends all listed player in this channel
     * the given message from the given chatter.
     * @param sender the chatter that performed the chat action.
     * @param message the message from the chatter.
     */
    void performChat(@NotNull final IChatter sender, @NotNull final String message);
}
