package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * IChatter Interface that defines a online chatter representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: July 12th, 2019
 * changed: July 5th, 2020
 */
public interface IChatter extends ICommandSource, IFilterable, IForcible, IOfflineChatter, Comparable<IChatter> {

    /**
     * Returns the player of this chatter.
     * @return the player of this chatter.
     */
    @NotNull Player getPlayer();

    /**
     * Returns the "friendly" name to display of this chatter.
     * @return the display name of this chatter.
     */
    @NotNull String getDisplayName();

    // Active Channel Methods:
    /**
     * Returns the currently channel in that this chatter is focused.
     * @return the currently focused channel of this chatter.
     */
    @NotNull IChannel getFocus();

    /**
     * Sets a new channel as the currently focused channel of this chatter. Can be null when the default channel should
     * be the new focused channel.
     * The old focused channel will be saved as 'last-focused-channel' when it is a non conversation channel, and will
     * be saved as 'last-persist-channel' when it is a persistent channel.
     * @param channel the new channel that should be the currently focused channel of this chatter, or null.
     * @return true when the currently focused channel was changed as result of this call, false otherwise.
     */
    boolean setFocus(@Nullable final IChannel channel);

    // Last Sources Methods:
    /**
     * Returns the last channel that this chatter had focused. Can be null when there is no last focused channel.
     * This Channel can be any non conversation channel.
     * @return the last focused channel of this chatter. or null when there is no last focused channel.
     */
    @SuppressWarnings("unused")
    @Nullable IChannel getLastFocused();

    /**
     * Returns the last persist channel that this chatter had focused. Can be null when there is no last focused
     * persist channel.
     * This Channel can only be persist channels.
     * @return the last focused persist channel of this chatter or null when there is no last focused channel.
     */
    @SuppressWarnings("unused")
    @Nullable IChannel getLastPersist();

    /**
     * Returns the last partner this chatter had a conversation with. Can be null when there is no last conversation
     * this chatter had
     * @return the last conversation partner.
     */
    @Nullable IChatter getLastPartner();

    /**
     * Sets a new chatters this chatter had a conversation with.
     * @param partner the new last conversation partner.
     * @return true when the last conversation partner was changed as result of this call, false otherwise.
     */
    boolean setLastPartner(@NotNull final IChatter partner);

    // Channels Collection Methods:

    /**
     * Returns all channel that this chatter is owning.
     * Can be all types instead of {@link ChannelType#CONVERSATION} of channels.
     * @return the channels this chatter is owning.
     */
    @NotNull Set<IChannel> getOwningChannels();

    /**
     * Returns all channel that this chatter is in. Can be all types of channels.
     * @return the channels this chatter is in.
     */
    @NotNull Set<IChannel> getChannels();

    /**
     * Joins a new channel. Adds the given channel to this chatter and calls {@link IChannel#addMember(IChatter)}.
     * @param channel the channel to join.
     * @return true when the channel was joined as result of this call, false otherwise.
     */
    boolean joinChannel(@NotNull final IChannel channel);

    /**
     * Leaves a channel. Removes the given channel from this chatter and calls {@link IChannel#removeMember(IChatter)}.
     * @param channel the channel to leave.
     * @return true when the channel was left as result of this call, false otherwise.
     */
    boolean leaveChannel(@NotNull final IChannel channel);

    /**
     * Returns whether this chatter contains the given channel or not.
     * @param channel the channel that should be checked.
     * @return true when the collection of channels contains given the channel, false otherwise.
     */
    boolean hasChannel(@NotNull final IChannel channel);

    // Ignored Chatter Collection Methods:
    /**
     * Adds a ignored player for this chatter.
     * @param uniqueId the uniqueId of the player to add.
     * @return true when the player was ignored as result of this call, false otherwise.
     */
    boolean addIgnore(@NotNull final UUID uniqueId);

    /**
     * Removes a ignored player from this chatter.
     * @param uniqueId the uniqueId of the player to remove.
     * @return true when the player was unignored as result of this call, false otherwise.
     */
    boolean removeIgnore(@NotNull final UUID uniqueId);

    /**
     * Returns whether the given uniqueId is ignored from this chatter.
     * @param uniqueId the uniqueId of the player to check.
     * @return true when the given uniqueId is ignored, false otherwise.
     */
    boolean isIgnore(@NotNull final UUID uniqueId);

    /**
     * Returns whether this chatter is ignoring someone.
     * @return true when someone is ignored, false otherwise.
     */
    boolean isIgnore();
}
