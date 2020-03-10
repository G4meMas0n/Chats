package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.messaging.Placeholder;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Channel Interface that defines a channel representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * changed: March 9th, 2020
 */
public interface IChannel extends Comparable<IChannel> {

    // Channel Properties Methods:
    /**
     * Returns the full name of this channel.
     * @return the full name.
     */
    @NotNull String getFullName();

    /**
     * Returns the full name colored with the chat color of this channel.
     * @return the colored full name.
     */
    @NotNull String getColoredName();

    /**
     * Returns the short name of this channel.
     * When this channel has no short name, the full name of this channel will be returned.
     * @return the short name.
     */
    @NotNull String getShortName();

    /**
     * Sets a new short name for this channel. Removes the short name when the given name is null or empty.
     * @param name the new short name for this channel.
     * @return true when the short name was changed as result of this call,
     *         false when the short name was not changed or when this channel do not support this feature.
     */
    boolean setShortName(@Nullable final String name);

    /**
     * Returns the chat color of this channel.
     * When this channel has no chat color, the default chat color will be returned.
     * @return the chat color.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets a new chat color for this channel. Removes the chat color when the given color is null.
     * @param color the new chat color for this channel.
     * @return true when the chat color was changed as result of this call,
     *         false when the chat color was not changed or when this channel do not support this feature.
     * @throws IllegalArgumentException Thrown when the given chat color is not a color.
     */
    boolean setChatColor(@Nullable final ChatColor color) throws IllegalArgumentException;

    /**
     * Returns the password of this channel. Can be null when this channel has no password.
     * @return the password or null.
     */
    @Nullable String getPassword();

    /**
     * Returns whether this channel has a password or not.
     * @return true when a valid password is set, false otherwise.
     */
    boolean hasPassword();

    /**
     * Sets a new password for this channel. Removes it when the given password is null or empty.
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call,
     *         false when the password was not changed or when this channel does not support this feature.
     * @throws IllegalArgumentException Thrown when the given password is not valid.
     */
    boolean setPassword(@Nullable final String password) throws IllegalArgumentException;

    /**
     * Returns whether this channel is cross world or not.
     * @return true when this channel is world across.
     */
    boolean isCrossWorld();

    /**
     * Sets whether this channel is cross world or not.
     * @param crossWorld true if this channel is world across.
     * @return true when the option was changed as result of this call,
     *         false when the option was not changed or when this channel does not support this feature.
     */
    boolean setCrossWorld(final boolean crossWorld);

    /**
     * Returns the distance of this channel.
     * When this channel has no distance, -1 will be returned.
     * @return the distance.
     */
    int getDistance();

    /**
     * Returns whether the distance of this channel is bigger than zero.
     * @return true when this channel has a distance, false otherwise.
     */
    boolean hasDistance();

    /**
     * Sets a new distance for this channel. Removes it when the given distance is not bigger than zero.
     * @param distance the new distance for this channel.
     * @return true when the distance was changed as result of this call.
     *         false when the distance was not changed or when this channel does not support this feature.
     */
    boolean setDistance(final int distance);

    // Channel Type Methods:
    /**
     * Returns the type of this channel.
     * @return the type.
     */
    @NotNull ChannelType getType();

    /**
     * Returns whether this channel represents a conversion channel or not.
     * @return true when it represents a conversation channel, false otherwise.
     */
    boolean isConversation();

    /**
     * Returns whether this channel represents a persist channel or not.
     * @return true when it represents a persist channel, false otherwise.
     */
    boolean isPersist();

    // Member, Banned, Muted, Moderator and Owner Methods:
    /**
     * Returns a copy of all members of this channel.
     * @return the chatters of all members.
     */
    @NotNull Set<IChatter> getMembers();

    /**
     * Sets a member for this channel.
     * @param chatter the chatter to set.
     * @param member true to add the chatter, false to remove.
     * @return true when the chatter was added as result of this call, false otherwise.
     */
    boolean setMember(@NotNull final IChatter chatter, final boolean member);

    /**
     * Adds a new member to this channel.
     * This method will add this channel to the given chatter when it is not already added.
     * @param chatter the member to add.
     * @return true when the member was added as result of this call,
     *         false when the members was not changed or when this channel does not support this feature.
     */
    boolean addMember(@NotNull final IChatter chatter);

    /**
     * Removes a member from this channel.
     * This method will remove this channel from the given chatter when it is not already removed.
     * @param chatter the member to remove.
     * @return true when the member was removed as result of this call,
     *         false when the members was not changed or when this channel does not support this feature.
     */
    boolean removeMember(@NotNull final IChatter chatter);

    /**
     * Returns whether the given chatter is a member of this channel or not.
     * @param chatter the chatter to check.
     * @return true when the given chatter is a member, false otherwise.
     */
    boolean isMember(@NotNull final IChatter chatter);

    /**
     * Returns a copy of all uniqueIds of the banned members of this channel.
     * @return the uniqueIds of all banned members.
     */
    @NotNull Set<UUID> getBanned();

    /**
     * Sets a banned chatter for this channel.
     * @param uniqueId the uniqueId of the chatter to set.
     * @param banned true to ban the chatter, false to pardon.
     * @return true when the chatter was banned as result of this call, false otherwise.
     */
    boolean setBanned(@NotNull final UUID uniqueId, final boolean banned);

    /**
     * Bans a member from this channel. Removes the given chatter and adds it to the ban list.
     * This method will remove this channel from the given chatter when it is not already removed.
     * Note that banned members are no longer able to join this channel again.
     * @param chatter the member to ban.
     * @return true when the given chatter was banned as result of this call,
     *         false when the given chatter is not a member or when this channel does not support this feature.
     */
    boolean banMember(@NotNull final IChatter chatter);

    /**
     * Pardons a banned member of this channel. Removes the given chatter from the ban list.
     * @param chatter the banned member to pardon.
     * @return true when the given chatter was pardoned as result of this call,
     *         false when the given chatter was not banned or when this channel does not support this feature.
     */
    boolean unBanMember(@NotNull final IChatter chatter);

    /**
     * Returns whether the given chatter is banned from this channel or not.
     * @param chatter the chatter to check.
     * @return true when the given chatter is banned, false otherwise.
     */
    boolean isBanned(@NotNull final IChatter chatter);

    /**
     * Kicks a member from this channel.
     * This method will remove this channel from the given chatter when it is not already removed.
     * @param chatter the member to kick.
     * @return true when the member was kicked as result of this call,
     *         false when the member is not a member or when this channel does not support this feature.
     */
    boolean kickMember(@NotNull final IChatter chatter);

    /**
     * Returns a copy of all uniqueIds of the muted members of this channel.
     * @return the uniqueIds of all muted members.
     */
    @NotNull Set<UUID> getMuted();

    /**
     * Sets a muted chatters for this channel.
     * @param uniqueId the uniqueId of the chatter to set.
     * @param muted true to mute the chatter, false to remove the mute.
     * @return true when the chatter was muted as result of this call, false otherwise.
     */
    boolean setMuted(@NotNull final UUID uniqueId, final boolean muted);

    /**
     * Mutes a member of this channel. Adds the given chatter to the mute list.
     * Note that muted members are no longer able to speak in this channel again.
     * @param chatter the member to mute.
     * @return true when the member was muted as result of this call,
     *         false when the member is not a member or when this channel does not support this feature.
     */
    boolean muteMember(@NotNull final IChatter chatter);

    /**
     * Un-mutes a muted member of this channel. Removed the given chatter from the mute list.
     * @param chatter the muted member to un-mute.
     * @return true when the member was un-muted as result of this call,
     *         false when the member was not muted or when this channel does not support this feature.
     */
    boolean unMuteMember(@NotNull final IChatter chatter);

    /**
     * Returns whether the given chatter is muted or not.
     * @param chatter the chatter to check.
     * @return true when the given chatter is muted, false otherwise.
     */
    boolean isMuted(@NotNull final IChatter chatter);

    /**
     * Returns a copy of all uniqueIds of the moderators of this channel.
     * @return the uniqueIds of all moderators.
     */
    @NotNull Set<UUID> getModerators();

    /**
     * Sets a moderator for this channel.
     * @param uniqueId the uniqueId of the moderator to set.
     * @param moderator true to add the moderator, false to remove.
     * @return true when the uniqueId of the moderator was added as result of this call, false otherwise.
     */
    boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator);

    /**
     * Adds a new moderator to this channel.
     * @param chatter the moderator to add.
     * @return true when the moderator was added as result of this call,
     *         false when the collection was not changed or when this channel does not support this feature.
     */
    boolean addModerator(@NotNull final IChatter chatter);

    /**
     * Removes a moderator from this channel.
     * @param chatter the moderator to remove.
     * @return true when the moderator was removed as result of this call,
     *         false when the collections was not changed or when this channel does not support this feature.
     */
    boolean removeModerator(@NotNull final IChatter chatter);

    /**
     * Returns whether the given uniqueId is a moderator of this channel.
     * @param chatter the chatter/player to check.
     * @return true when the given uniqueId is a moderator, false otherwise.
     */
    boolean isModerator(@NotNull final IChatter chatter);

    /**
     * Returns the owner of this channel. Can be null when this channel has no owner.
     * @return the uniqueId of the owner.
     */
    @Nullable UUID getOwner();

    /**
     * Returns whether this channel has a owner or not.
     * @return true when a owner is set, false otherwise.
     */
    boolean hasOwner();

    /**
     * Sets the owner of this channel. Removes the owner when the given uniqueId is null.
     * @param uniqueId the uniqueId of the owner to set.
     * @return true when the owner was changed as result of this call,
     *         false when the owner was not changed or when this channel does not support this feature.
     */
    boolean setOwner(@Nullable final UUID uniqueId);

    /**
     * Returns whether the given uniqueId is the owner of this channel.
     * @param uniqueId the uniqueId of the chatter/player to check.
     * @return true when the given uniqueId is the owner, false otherwise.
     */
    boolean isOwner(@NotNull final UUID uniqueId);

    // Formatting Methods:
    @NotNull IFormatter getFormatter();

    /**
     * Returns the announce format of this channel.
     * When this channel uses custom formats, the custom announce format will be returned if it exists.
     * Otherwise it returns the default announce format.
     * @return the announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the custom announce format for this channel. Removes it when the given format is null or empty.
     * Throws an exception when the given format does not include the {@link Placeholder#MESSAGE} placeholder.
     * @param format the new announce format for this channel.
     * @return true when the format was changed as result of this call,
     *         false when the format was not changed or when this channel does not support this feature.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the broadcast format of this channel.
     * When this channel uses custom formats, the custom broadcast format will be returned if it exists.
     * Otherwise it returns the default broadcast format.
     * @return the broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the custom broadcast format for this channel. Removes it when the given format is null or empty.
     * Thrown an exception when the given format does not include the {@link Placeholder#BROADCAST_PREFIX} or
     * {@link Placeholder#MESSAGE} placeholders.
     * @param format the new broadcast format for this channel.
     * @return true when the format was changed as result of this call,
     *         false when the format was not changed or when this channel does not support this feature.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the chat format that of this channel.
     * When this channel uses custom formats, the custom chat format will be returned if it exists.
     * Otherwise it returns the default chat format.
     * @return the chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Sets the chat format for this channel. Removes it when the given format is null or empty.
     * Throws an exception when the given format does not include the {@link Placeholder#SENDER},
     * {@link Placeholder#SENDER_PLAIN} or the {@link Placeholder#MESSAGE} placeholders.
     * @param format the new channel format for this channel.
     * @return true when the format was changed as result of this call,
     *         false when the format was not changed or when this channel does not support this feature.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setChatFormat(@Nullable final String format);

    /**
     * Returns whether this channel uses custom formats or not.
     * @return true when it uses custom formats.
     */
    boolean isCustomFormat();

    /**
     * Sets whether this channel uses custom formats or the default formats.
     * @param customFormat true when this channel uses custom formats.
     * @return true when the option was changed as result of this call,
     *         false when the option was not changed or when this channel does not support this feature.
     */
    boolean setCustomFormat(final boolean customFormat);

    // Performing Methods:
    /**
     * Performs the announce action. This method will run this action synchronized.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given announce message.
     * @param message the announce message.
     */
    void performAnnounce(@NotNull final String message);

    /**
     * Performs the broadcast action. This method will run this action synchronized.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given broadcast message.
     * @param message the broadcast message.
     */
    void performBroadcast(@NotNull final String message);

    /**
     * Performs the chat action. This method will run this action synchronized.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel that can
     * see the given chat message from the given sender.
     * @param sender the sender of the given message.
     * @param message the chat message from the sender.
     */
    void performChat(@NotNull final IChatter sender, @NotNull final String message);

    // FullName buildings methods:
    /**
     * Builds the full name for conversation channels.
     * The full name is build of the uniqueId of both players concatenated with a underscore ("_").
     * The order in which the chatters are specified as arguments does not matter as they are compared before. So
     * buildConversationName(first, second) will return the same as buildConversationName(second, first).
     * @param first the first chatter to build the full name of the conversation.
     * @param second the second chatter to build the full name of the conversation.
     * @return the full name of the conversation for the given chatters.
     */
    static @NotNull String buildConversationName(@NotNull final IChatter first, @NotNull final IChatter second) {
        if (first.compareTo(second) >= 0) {
            return first.getPlayer().getUniqueId() + "_" + second.getPlayer().getUniqueId();
        } else {
            return second.getPlayer().getUniqueId() + "_" + first.getPlayer().getUniqueId();
        }
    }
}
