package de.g4memas0n.Chats.channels;

import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.utils.Placeholder;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;

public interface IChannel {
    String DEFAULT_CHANNEL_FORMAT = Placeholder.CHANNEL_COLOR.toString() + Placeholder.CHANNEL_NICK.toString()
                                    + Placeholder.SENDER.toString() + Placeholder.CHANNEL_COLOR.toString() + ": "
                                    + Placeholder.MESSAGE.toString();
    String DEFAULT_ANNOUNCE_FORMAT = Placeholder.CHANNEL_COLOR.toString() + Placeholder.MESSAGE.toString();
    String DEFAULT_BROADCAST_FORMAT = Placeholder.CHANNEL_COLOR.toString() + "["
                                      + Placeholder.BROADCAST_PREFIX.toString() + Placeholder.CHANNEL_COLOR.toString()
                                      + "] " + Placeholder.MESSAGE.toString();
    String DEFAULT_CONVERSION_FORMAT = Placeholder.CHANNEL_COLOR.toString() + "["
                                       + Placeholder.CONVERSION_ADDRESS.toString() + " "
                                       + Placeholder.CONVERSION_PARTNER.toString()
                                       + Placeholder.CHANNEL_COLOR.toString() + "] " + Placeholder.MESSAGE.toString();

    /**
     * Returns the listed full name of this channel.
     * @return the full name of this channel.
     */
    @NotNull String getFullName();

    /**
     * Returns the channel type of this channel.
     * @return true if this channel is a global channel.
     */
    boolean isGlobalChannel();

    /**
     * Returns the channel type of this channel.
     * @return true if this channel is a conversion channel.
     */
    boolean isConversionChannel();


    // Channel Cross World Methods:
    /**
     * Returns if this channel is worlds across.
     * @return true if this channel is world across.
     */
    boolean isCrossWorld();

    /**
     * Sets the across world option of this channel.
     * @param state if this channel should be across worlds.
     * @return true if the across world option was changed as result of this call.
     */
    boolean setCrossWorld(final boolean state);


    // Channel Short Name Methods:
    /**
     * Returns the listed short name of this channel.
     * @return the short name of this channel.
     */
    @NotNull String getShortName();

    /**
     * Sets a new short name for this channel.
     * @param shortName the new short name for this channel.
     * @return true if the short name was changed as result of this call.
     */
    boolean setShortName(@NotNull final String shortName);


    // Channel Color Methods:
    /**
     * Returns the listed chat color of this channel.
     * @return the color of this channel.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets a new color for this channel.
     * @param chatColor the new chat color for this channel.
     * @return true if the channel color was changed as result of this call.
     */
    boolean setChatColor(@NotNull final ChatColor chatColor);


    // Channel Password Methods:
    /**
     * Returns if this channel has set a password or not.
     * @return true if the channel has set a password and false if the channel has not set a password.
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
     * @return true if the password was changed as result of this call.
     */
    boolean setPassword(@NotNull final String password);

    /**
     * Removes the password from this channel.
     * @return true if the password was removed as result of this call.
     */
    boolean removePassword();


    // Channel Format Methods:
    /**
     * Returns if this channel use custom format or use the default format.
     * @return true if this channel use a custom format.
     */
    boolean isUseCustomFormat();

    /**
     * Sets the use custom format option for this channel.
     * @param state if this channel should use a custom format.
     * @return true if the use custom format option was changed as result of this call.
     */
    boolean setUseCustomFormat(final boolean state);

    /**
     * Returns the channel format of this channel.
     * @return the channel format of this channel.
     */
    @NotNull
    String getChannelFormat();

    /**
     * Sets the channel format for this channel.
     * @param format the new channel format for this channel.
     * @return true if the channel format was changed as result of this call.
     */
    boolean setChannelFormat(@NotNull final String format);

    /**
     * Returns the announce format of this channel.
     * @return the announce format of this channel.
     */
    @NotNull
    String getAnnounceFormat();

    /**
     * Sets the announce format for this channel.
     * @param format the new announce format for this channel.
     * @return true if the announce format was changed as result of this call.
     */
    boolean setAnnounceFormat(@NotNull final String format);

    /**
     * Returns the broadcast format of this channel.
     * @return the broadcast format of this channel.
     */
    @NotNull
    String getBroadcastFormat();

    /**
     * Sets the broadcast format for this channel.
     * @param format the new broadcast format for this channel.
     * @return true if the broadcast format was changed as result of this call.
     */
    boolean setBroadcastFormat(@NotNull final String format);


    // Chatter Collection Methods:
    /**
     * Returns all listed chatters of this channel.
     * @return all chatters listed in this channel.
     */
    @NotNull Collection<IChatter> getChatters();

    /**
     * Adds a new chatter to this channel.
     * @param chatter a chatter who should be listed in this channel.
     * @return true if the collection changed as result of this call.
     */
    boolean addChatter(@NotNull final IChatter chatter);

    /**
     * Removes a chatter of this channel.
     * @param chatter a chatter who should be removed from this channel.
     * @return true if the chatter was removed as result of this call.
     */
    boolean removeChatter(@NotNull final IChatter chatter);


    // Perform Chat Method:
    /**
     * Perform the chat action for this channel.
     * Checks all conditions to perform the chat action successfully and then sends all listed player in this channel
     * the given message from the given chatter.
     * @param chatter the chatter that performed the chat action.
     * @param message the message from the chatter.
     */
    void performChat(@NotNull final IChatter chatter, @NotNull final String message);
}