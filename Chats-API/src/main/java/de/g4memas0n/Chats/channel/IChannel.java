package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.storage.IStorageHolder;
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
 * changed: February 3rd, 2020
 */
public interface IChannel extends IStorageHolder, Comparable<IChannel> {

    // Channel Properties Methods:
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
     * Sets a new short name for this channel or removes it when the given argument is null or empty.
     * @param name the new short name for this channel.
     * @return true when the short name was changed as result of this call.
     *         false when the short name was not changed or when this channel do not support this feature.
     */
    boolean setShortName(@Nullable final String name);

    /**
     * Returns the listed chat color of this channel.
     * @return the color of this channel.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets a new chat color for this channel or removes it when the given argument is null.
     * @param color the new chat color for this channel.
     * @return true when this channel color was changed as result of this call.
     *         false when the color was not changed or when this channel do not support this feature.
     */
    boolean setChatColor(@Nullable final ChatColor color);

    /**
     * Returns if this channel has set a password or not.
     * @return true when a valid password is set.
     *         false when no password is set or when the password is empty.
     */
    boolean hasPassword();

    /**
     * Returns the password of this channel.
     * @return the password of this channel or null when no password is set.
     */
    @Nullable String getPassword();

    /**
     * Sets a new password for this channel or removes it when the given argument is null or empty.
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call.
     *         false when the password was not changed or when this channel do not support this feature.
     */
    boolean setPassword(@Nullable final String password);

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

    // Channel Type Methods:
    /**
     * Returns the type of this channel.
     * @return the channel type.
     */
    @NotNull ChannelType getTpe();

    /**
     * Returns whether this channel represents a conversion channel or not.
     * @return true when this channel represents a conversion channel, false otherwise.
     */
    boolean isConversation();

    /**
     * Returns whether this channel represents a persist channel or not.
     * @return true when this channel represents a persist channel, false otherwise.
     */
    boolean isPersist();

    // Chatter Collection Methods:
    /**
     * Returns a copy of all chatter that are registered in this channel.
     * @return a set of all registered chatters.
     */
    @NotNull Set<IChatter> getChatters();

    /**
     * Registers (Adds) a new chatter to this channel.
     * This method only adds the given chatter to this channel and do not add this channel to the given chatter.
     * @param chatter a chatter who should be registered in this channel.
     * @return true when the collection was changed as result of this call.
     *         false when the collection was not changed or when this channel do not support this feature.
     */
    boolean addChatter(@NotNull final IChatter chatter);

    /**
     * Unregisters (Removes) a chatter from this channel.
     * This method only removes the given chatter from this channel and do not remove this channel from the given
     * chatter.
     * @param chatter a chatter who should be unregistered from this channel.
     * @return true when the chatter was removed as result of this call.
     *         false when the collection was not changed or when this channel do not support this feature.
     */
    boolean removeChatter(@NotNull final IChatter chatter);

    /**
     * Returns whether this channel contains the given chatter or not.
     * @param chatter the chatter that should be checked.
     * @return true when this channel contains the chatter, false otherwise.
     */
    boolean hasChatter(@NotNull final IChatter chatter);

    // Channel Formatter and Performer Methods:
    /**
     * Returns the announce format that is used for this channel.
     * When the 'use-custom-format' option is active then it will return the custom announce format if it is specified.
     * Otherwise this method returns the default announce format that is specified in the formatter of this channel.
     * @return the announce format that is used for this channel.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the custom announce format for this channel or removes it when the given format is null or empty.
     * @param format the new announce format for this channel.
     * @return true when the announce format was changed as result of this call.
     *         false when the format was not changed or when this channel do not support this feature.
     * @throws IllegalArgumentException Thrown when the given format do not include the message placeholder.
     */
    boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the broadcast format that is used for this channel.
     * When the 'use-custom-format' option is active then it will return the custom broadcast format if it is specified.
     * Otherwise this method returns the default broadcast format that is specified in the formatter of this channel.
     * @return the broadcast format that is used for this channel.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the custom broadcast format for this channel or removes it when the given format is null or empty.
     * @param format the new broadcast format for this channel.
     * @return true when the broadcast format was changed as result of this call.
     *         false when the format was not changed or when this channel do not support this feature.
     * @throws IllegalArgumentException Thrown when the given format do not include the message placeholder.
     */
    boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the chat format that is used for this channel.
     * When the 'use-custom-format' option is active then it will return the custom chat format if it is specified.
     * Otherwise this method returns the default chat format that is specified in the formatter of this channel.
     * @return the chat format that is used for this channel.
     */
    @NotNull String getChatFormat();

    /**
     * Sets the chat format for this channel.
     * @param format the new channel format.
     * @return true when the channel format was changed as result of this call.
     *         false when the format was not changed.
     * @throws IllegalArgumentException Thrown when the given format do not include the sender and message placeholder.
     */
    boolean setChatFormat(@Nullable final String format);

    /**
     * Returns whether this channel uses custom formats or the default formats.
     * When this option is active this channel will use the custom format when it is specified, otherwise this method
     * will return the default formats that are specified in the formatter of this channel.
     * @return true when this channel use a custom format, false otherwise.
     */
    boolean isUseCustomFormat();

    /**
     * Sets whether this channel uses custom formats or the default formats.
     * When this option is active this channel will use the custom format when it is specified, otherwise this method
     * will return the default formats that are specified in the formatter of this channel.
     * @param state whether this channel should use custom formats.
     * @return true when the use custom format option was changed as result of this call.
     *         false when the option was not changed or when this channel do not support this feature.
     */
    boolean setUseCustomFormat(final boolean state);

    /**
     * Returns the message formatter that is used to format all messages.
     * @return the used message formatter of this channel.
     */
    @NotNull IChatFormatter getFormatter();

    /**
     * Returns the action performer that is used to perform all channel actions.
     * @return the used action performer of this channel.
     */
    @NotNull IChatPerformer getPerformer();
}
