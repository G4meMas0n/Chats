package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.storage.IStorageHolder;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Chatter Interface that defines a chatter representation, extends {@link IPermissible}, {@link IFilterable},
 * {@link IStorageHolder} and {@link Comparable}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 12th, 2019
 * changed: March 9th, 2020
 */
public interface IChatter extends IPermissible, IFilterable, IStorageHolder, Comparable<IChatter> {

    /**
     * Returns the listed Player of this chatter.
     * @return the player of this chatter.
     */
    @NotNull Player getPlayer();

    // Active Channel Methods:
    /**
     * Returns the currently channel in that this chatter is focused.
     * @return the currently focused channel of this chatter.
     */
    @NotNull IChannel getFocus();

    /**
     * Sets a new channel as the currently focused channel of this chatter.
     * The old focused channel will be saved as 'last-focused-channel' when it is a non conversation channel.
     * When the old focused channel is a persist channel then it will additionally saved as 'last-persist-channel'.
     * @param channel the new channel that should be the currently focused channel of this chatter.
     * @return true when the currently focused channel was changed as result of this call, false otherwise.
     */
    boolean setFocus(@NotNull final IChannel channel);

    // Last Sources Methods:
    /**
     * Returns the last channel that this chatter had focused. Can be null when there is no last focused channel.
     * This Channel can be any non conversation channel.
     * @return the last focused channel of this chatter. or null when there is no last focused channel.
     */
    @Nullable IChannel getLastFocused();

    /**
     * Returns the last persist channel that this chatter had focused. Can be null when there is no last focused
     * persist channel.
     * This Channel can only be persist channels.
     * @return the last focused persist channel of this chatter or null when there is no last focused channel.
     */
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
    boolean setLastPartners(@NotNull final IChatter partner);

    // Channels Collection Methods:
    /**
     * Returns all channel that this chatter is in. Can be all types of channels.
     * @return a copy of a set of channels this chatter is in.
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
     * Returns all UUIDs of chatters this chatter is ignoring.
     * @return a copy of a set of chatter UUIDs this chatter is ignoring.
     */
    @NotNull Set<UUID> getIgnores();

    /**
     * Adds a new UUID of a chatter to the collection of ignoring chatter UUIDs of this chatter.
     * @param uuid the UUID of a chatter that should be ignored from this chatter.
     * @return true when the collection of ignoring chatter UUIDs was updated as result of this call, false otherwise.
     */
    boolean addIgnores(@NotNull final UUID uuid);

    /**
     * Removes a given UUID of a chatter from the collection of ignoring chatter UUIDs of this chatter.
     * @param uuid the UUID of a chatter that should be no longer ignored from this chatter.
     * @return true when the collection of ignoring chatter UUIDs was updated as result of this call, false otherwise.
     */
    boolean removeIgnores(@NotNull final UUID uuid);

    /**
     * Returns whether the given UUID of a chatter is ignored from this chatter.
     * @param uuid the UUID of a chatter that should be checked.
     * @return true when the collection of ignoring chatter UUIDs contains the given UUID of a chatter, false otherwise.
     */
    boolean isIgnoring(@NotNull final UUID uuid);

    /**
     * Returns whether this chatter is ignoring someone.
     * @return true when the collection of ignoring chatter UUIDs is not empty, false otherwise.
     */
    boolean isIgnoring();
}
