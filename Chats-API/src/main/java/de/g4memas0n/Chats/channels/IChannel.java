package de.g4memas0n.Chats.channels;

import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.formatters.Placeholder;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;

public interface IChannel {
    String DEFAULT_ANNOUNCE_FORMAT = Placeholder.CHANNEL_COLOR.toString() + Placeholder.MESSAGE.toString();
    String DEFAULT_BROADCAST_FORMAT = Placeholder.CHANNEL_COLOR.toString() + "["
                                      + Placeholder.BROADCAST_PREFIX.toString() + Placeholder.CHANNEL_COLOR.toString()
                                      + "] " + Placeholder.MESSAGE.toString();
    String DEFAULT_CHANNEL_FORMAT = Placeholder.CHANNEL_COLOR.toString() + Placeholder.CHANNEL_NICK.toString()
                                    + Placeholder.SENDER.toString() + Placeholder.CHANNEL_COLOR.toString() + ": "
                                    + Placeholder.MESSAGE.toString();
    String DEFAULT_CONVERSION_FORMAT = Placeholder.CHANNEL_COLOR.toString() + "[" + Placeholder.SENDER.toString()
                                       + " -> " + Placeholder.CON_PARTNER.toString()
                                       + Placeholder.CHANNEL_COLOR.toString() + "] " + Placeholder.MESSAGE.toString();
    String DEFAULT_CONVERSION_TWITTER_FORMAT = Placeholder.CHANNEL_COLOR.toString() + Placeholder.CON_ADDRESS.toString() + " "
                                       + Placeholder.CON_PARTNER.toString() + Placeholder.CHANNEL_COLOR.toString()
                                       + ": " + Placeholder.MESSAGE.toString();

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
     * @param shortName the new short name for this channel.
     * @return true when the short name was changed as result of this call.
     */
    boolean setShortName(@NotNull final String shortName);

    /**
     * Returns the listed chat color of this channel.
     * @return the color of this channel.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets a new color for this channel.
     * @param chatColor the new chat color for this channel.
     * @return true when this channel color was changed as result of this call.
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
     */
    boolean setDistance(final int distance);

    /**
     * Returns if this channel has set a password or not.
     * @return true when this channel has set a password and false if the channel has not set a password.
     */
    boolean hasPassword();

    /**
     * Returns the password of this channel.
     * @return the password of this channel or null if no password is set.
     */
    @Nullable String getPassword();

    /**
     * Sets a new password for this channel.
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call.
     */
    boolean setPassword(@NotNull final String password);

    /**
     * Removes the password from this channel.
     * @return true when the password was removed as result of this call.
     */
    boolean removePassword();

    // Methods for Custom Channel Formats:
    /**
     * Returns if this channel use custom format or use the default format.
     * @return true when this channel use a custom format.
     */
    boolean isUseCustomFormat();

    /**
     * Sets the use custom format option for this channel.
     * @param state if this channel should use a custom format.
     * @return true when the use custom format option was changed as result of this call.
     */
    boolean setUseCustomFormat(final boolean state);

    /**
     * Returns the announce format of this channel.
     * @return the announce format of this channel.
     */
    @NotNull
    String getCustomAnnounceFormat();

    /**
     * Sets the announce format for this channel.
     * @param format the new announce format for this channel.
     * @return true when the announce format was changed as result of this call.
     */
    boolean setCustomAnnounceFormat(@NotNull final String format);

    /**
     * Returns the broadcast format of this channel.
     * @return the broadcast format of this channel.
     */
    @NotNull
    String getCustomBroadcastFormat();

    /**
     * Sets the broadcast format for this channel.
     * @param format the new broadcast format for this channel.
     * @return true when the broadcast format was changed as result of this call.
     */
    boolean setCustomBroadcastFormat(@NotNull final String format);

    /**
     * Returns the channel format of this channel.
     * @return the channel format of this channel.
     */
    @NotNull
    String getCustomChannelFormat();

    /**
     * Sets the channel format for this channel.
     * @param format the new channel format for this channel.
     * @return true when the channel format was changed as result of this call.
     */
    boolean setCustomChannelFormat(@NotNull final String format);

    // Methods for Chatter Collection of this Channel:
    /**
     * Returns all listed chatters of this channel.
     * @return all chatters listed in this channel.
     */
    @NotNull Collection<IChatter> getChatters();

    /**
     * Adds a new chatter to this channel.
     * @param chatter a chatter who should be listed in this channel.
     * @return true when the collection changed as result of this call.
     */
    boolean addChatter(@NotNull final IChatter chatter);

    /**
     * Removes a chatter of this channel.
     * @param chatter a chatter who should be removed from this channel.
     * @return true when the chatter was removed as result of this call.
     */
    boolean removeChatter(@NotNull final IChatter chatter);

    // Method for performing Chat:
    /**
     * Perform the chat action for this channel.
     * Checks all conditions to perform the chat action successfully and then sends all listed player in this channel
     * the given message from the given chatter.
     * @param sender the chatter that performed the chat action.
     * @param message the message from the chatter.
     */
    void performChat(@NotNull final IChatter sender, @NotNull final String message);
}