package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Channel Interface that defines a channel representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: July 13th, 2019
 * changed: June 19th, 2020
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
     * @return true when the short name was changed as result of this call, false otherwise.
     */
    boolean setShortName(@Nullable final String name);

    /**
     * Returns the chat color of this channel.
     * @return the chat color.
     */
    @NotNull ChatColor getColor();

    /**
     * Sets a new chat color for this channel. Resets the chat color when the given color is null.
     * @param color the new chat color for this channel.
     * @return true when the chat color was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given chat color is not a color.
     */
    boolean setColor(@Nullable final ChatColor color) throws IllegalArgumentException;

    /**
     * Returns whether this channel has a password or not.
     * @return true when a valid password is set, false otherwise.
     */
    boolean hasPassword();

    /**
     * Returns the password of this channel.
     * @return the password or null when this channel has no password.
     */
    @Nullable String getPassword();

    /**
     * Sets a new password for this channel. Removes it when the given password is null or empty.
     * The given password must be at least three characters long, otherwise it will throw an exception.
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given password is shorter as the minimum character count.
     */
    boolean setPassword(@Nullable final String password) throws IllegalArgumentException;

    /**
     * Returns whether this channel has a distance.
     * @return true when the distance of this channel is bigger than zero, false otherwise.
     */
    boolean hasDistance();

    /**
     * Returns the distance of this channel.
     * @return the distance, or -1 when this channel has no distance.
     */
    int getDistance();

    /**
     * Sets a new distance for this channel. Removes it when the given distance is smaller than one.
     * @param distance the new distance for this channel.
     * @return true when the distance was changed as result of this call, false otherwise.
     */
    boolean setDistance(final int distance);

    /**
     * Returns whether this channel is cross world or not.
     * @return true when this channel is world across and do not have a distance, false otherwise.
     */
    boolean isCrossWorld();

    /**
     * Sets whether this channel is cross world or not.
     * @param crossWorld true if this channel is world across.
     * @return true when the option was changed as result of this call, false otherwise.
     */
    boolean setCrossWorld(final boolean crossWorld);

    /**
     * Returns whether this channel uses custom formats or not.
     * @return true when it uses custom formats, false otherwise.
     */
    boolean isCustomFormat();

    /**
     * Sets whether this channel uses custom formats or the default formats.
     * @param customFormat true when this channel uses custom formats.
     * @return true when the option was changed as result of this call, false otherwise.
     */
    boolean setCustomFormat(final boolean customFormat);

    /**
     * Returns the announce format of this channel.
     * @return the custom announce format if it exists, otherwise the default announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the custom announce format for this channel. Removes it when the given format is null or empty.
     * The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an exception.
     * @param format the new announce format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the broadcast format of this channel.
     * @return the custom broadcast format it it exists, otherwise the default broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the custom broadcast format for this channel. Removes it when the given format is null or empty.
     * The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an exception.
     * @param format the new broadcast format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the chat format that of this channel.
     * @return the custom chat format if it exists, otherwise the default chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Sets the custom chat format for this channel. Removes it when the given format is null or empty.
     * The given format must include the {@link Placeholder#MESSAGE} placeholder and one of the sender placeholders
     * {@link Placeholder#SENDER} or {@link Placeholder#SENDER_PLAIN}, otherwise it will throw an exception.
     * @param format the new channel format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException;


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

    /**
     * Returns whether this channel represents a standard channel or not.
     * @return true when it represents a standard channel, false otherwise.
     */
    boolean isStandard();

    /**
     * Returns whether this channel is the default channel or not.
     * @return true when it is the default channel, false otherwise.
     */
    boolean isDefault();

    // Channel Collection Methods:
    /**
     * Returns all members of this channel.
     * @return the members.
     */
    @NotNull Set<IChatter> getMembers();

    /**
     * Sets a member for this channel.
     *
     * Note: This method will not remove this channel from the given chatter and will not announce the leaving. Please
     * use the {@link IChannel#addMember(IChatter)} and {@link IChannel#removeMember(IChatter)} or the
     * {@link IChatter#joinChannel(IChannel)} and {@link IChatter#leaveChannel(IChannel)} methods.
     * @param chatter the chatter to set.
     * @param member true to add to members, false to remove from members.
     * @return true when the chatter was added or remove as result of this call, false otherwise.
     */
    boolean setMember(@NotNull final IChatter chatter, final boolean member);

    /**
     * Adds a new member to this channel.
     * This method will add this channel to the given chatter when it is not already added.
     * @param chatter the member to add.
     * @return true when the member was added as result of this call, false otherwise.
     */
    boolean addMember(@NotNull final IChatter chatter);

    /**
     * Removes a member from this channel.
     * This method will remove this channel from the given chatter when it is not already removed.
     * @param chatter the member to remove.
     * @return true when the member was removed as result of this call, false otherwise.
     */
    boolean removeMember(@NotNull final IChatter chatter);

    /**
     * Bans a member from this channel.
     * This method will remove this channel from the given chatter and bans the uniqueId of the given chatter from this
     * channel.
     * @param member the member to ban.
     * @return true when the member was banned as result of this call, false otherwise.
     */
    boolean banMember(@NotNull final IChatter member);

    /**
     * Pardons a banned chatter in this channel.
     * This method will pardon the banned uniqueId of the given offline chatter.
     * @param chatter the banned chatter to pardon.
     * @return true when the banned chatter was pardoned as result of this call, false otherwise.
     */
    boolean pardonMember(@NotNull final IOfflineChatter chatter);

    /**
     * Kicks a member from this channel.
     * This method will remove this channel from the given chatter when it is not already removed.
     * @param member the member to kick.
     * @return true when the member was kicked as result of this call, false otherwise.
     */
    boolean kickMember(@NotNull final IChatter member);

    /**
     * Mutes a member in this channel.
     * This method will mutes the given chatter in this channel, when it is not already muted.
     * @param member the member to mute.
     * @return true when the member was muted as result of this call, false otherwise.
     */
    boolean muteMember(@NotNull final IChatter member);

    /**
     * Unmutes a muted member in this channel.
     * This method will unmute the given channel in this channel, when it is not already unmuted.
     * @param member the muted member to unmute.
     * @return true when the member was unmuted as result of this call, false otherwise.
     */
    boolean unmuteMember(@NotNull final IOfflineChatter member);

    /**
     * Returns whether the given chatter is a member of this channel or not.
     * @param chatter the chatter to check.
     * @return true when the given chatter is a member, false otherwise.
     */
    boolean isMember(@NotNull final IChatter chatter);

    /**
     * Returns all banned members of this channel.
     * @return the uniqueIds of the banned members.
     */
    @NotNull Set<UUID> getBans();

    /**
     * Sets the banned members for this channel
     * @param bans the uniqueIds of the banned members.
     * @return true when the banned members has changed as result of this call, false otherwise.
     */
    boolean setBans(@NotNull final Set<UUID> bans);

    /**
     * Sets a banned member for this channel.
     * @param uniqueId the uniqueId of the member to set.
     * @param banned true to ban the member, false to pardon.
     * @return true when the member was banned or pardoned as result of this call, false otherwise.
     */
    boolean setBanned(@NotNull final UUID uniqueId, final boolean banned);

    /**
     * Returns whether the given uniqueId is banned from this channel or not.
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is banned, false otherwise.
     */
    boolean isBanned(@NotNull final UUID uniqueId);

    /**
     * Returns all moderators of this channel.
     * @return the uniqueIds of the moderators.
     */
    @NotNull Set<UUID> getModerators();

    /**
     * Sets the moderators for this channel.
     * @param moderators the uniqueIds of the moderators.
     * @return true when the moderators has changed as result of this call, false otherwise.
     */
    boolean setModerators(@NotNull final Set<UUID> moderators);

    /**
     * Sets a moderator for this channel.
     * @param uniqueId the uniqueId of the moderator to set.
     * @param moderator true to add the moderator, false to remove.
     * @return true when the moderator was added or removed as result of this call, false otherwise.
     */
    boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator);

    /**
     * Returns whether the given uniqueId is a moderator of this channel.
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is a moderator, false otherwise.
     */
    boolean isModerator(@NotNull final UUID uniqueId);

    /**
     * Returns all muted members of this channel.
     * @return the uniqueIds of the muted members.
     */
    @NotNull Set<UUID> getMutes();

    /**
     * Sets the muted members for this channel
     * @param mutes the uniqueIds of the muted members.
     * @return true when the muted members has changed as result of this call, false otherwise.
     */
    boolean setMutes(@NotNull final Set<UUID> mutes);

    /**
     * Sets a muted chatters for this channel.
     * @param uniqueId the uniqueId of the member to set.
     * @param muted true to mute the chatter, false to unmute.
     * @return true when the member was muted or unmuted as result of this call, false otherwise.
     */
    boolean setMuted(@NotNull final UUID uniqueId, final boolean muted);

    /**
     * Returns whether the given uniqueId is muted in this channel or not.
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is muted, false otherwise.
     */
    boolean isMuted(@NotNull final UUID uniqueId);

    /**
     * Returns whether this channel has a owner or not.
     * @return true when a owner is set, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean hasOwner();

    /**
     * Returns the owner of this channel.
     * @return the uniqueId of the owner, or null when this channel has no owner.
     */
    @Nullable UUID getOwner();

    /**
     * Sets the owner of this channel. Removes the owner when the given uniqueId is null.
     * @param uniqueId the uniqueId of the owner to set.
     * @return true when the owner was changed as result of this call, false otherwise.
     */
    boolean setOwner(@Nullable final UUID uniqueId);

    /**
     * Returns whether the given uniqueId is the owner of this channel.
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is the owner, false otherwise.
     */
    boolean isOwner(@NotNull final UUID uniqueId);

    // Performing Methods:
    /**
     * Performs the announce action. This method can be called asynchronous.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given announce message.
     * @param message the announce message.
     */
    void performAnnounce(@NotNull final String message);

    /**
     * Performs the broadcast action. This method can be called asynchronous.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given broadcast message.
     * @param message the broadcast message.
     */
    void performBroadcast(@NotNull final String message);

    /**
     * Performs the chat action. This method can be called asynchronous.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel that can
     * see the given chat message from the given sender.
     * @param sender the sender of the given message.
     * @param message the chat message from the sender.
     */
    void performChat(@NotNull final IChatter sender, @NotNull final String message);
}
