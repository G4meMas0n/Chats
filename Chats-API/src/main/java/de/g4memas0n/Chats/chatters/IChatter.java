package de.g4memas0n.Chats.chatters;

import de.g4memas0n.Chats.channels.IChannel;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.Collection;
import java.util.UUID;

public interface IChatter {

    /**
     * Returns the listed Player of this chatter.
     * @return the player of this chatter.
     */
    @NotNull Player getPlayer();


    // Active Channel Methods:
    /**
     * Returns the currently active channel of this chatter.
     * @return the active channel of this chatter.
     */
    @NotNull IChannel getActiveChannel();

    /**
     * Sets the current active channel of this chatter.
     * Sets the old active channel as last global channel if it was a global channel.
     * @param channel the new active channel for this chatter.
     * @return true if the active channel was changed as result of this call.
     */
    boolean setActiveChannel(@NotNull final IChannel channel);


    // Last Channels Methods:
    /**
     * Returns the last active global channel of this chatter.
     * @return the last global channel of this chatter.
     */
    @NotNull IChannel getLastGlobalChannel();

    /**
     * Returns the channel of the last conversion of this chatter.
     * @return the last conversion channel.
     */
    @NotNull IChatter getLastConversionChannel();

    /**
     * Sets the last conversion channel of this chatter.
     * @param channel the new last conversion channel for this chatter.
     * @return true if the last conversion channel was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is not a conversion channel.
     */
    boolean setLastConversionChannel(@NotNull final IChannel channel) throws IllegalArgumentException;


    // Channels Collection Methods:
    /**
     * Returns the collection of all channels this chatter is in.
     * @return all channels this chatter is in.
     */
    @NotNull Collection<IChannel> getChannels();

    /**
     * Adds a new channel to this chatter and also adds this chatter to the chatter list of the given channel.
     * @param channel the new channel that should be added to this chatter.
     * @return true if the channel collection of this chatter and the chatter list of the given channel was
     *              changed as result of this call.
     */
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes a channel from this chatter and also removes this chatter from the chatter list of the given channel.
     * @param channel the channel that should be removed from this chatter.
     * @return true if the channel collection of this chatter and the chatter list of the given channel was
     *              changed as result of this call.
     */
    boolean removeChannel(@NotNull final IChannel channel);


    // Ignored Chatter Collection Methods:
    /**
     * Returns the collection of all player UUIDs that this chatter is ignoring.
     * @return all ignored player UUIDs of this chatter.
     */
    @NotNull Collection<UUID> getIgnoredChatters();

    /**
     * Adds a new player UUID to the list of all ignoring player UUIDs of this chatter.
     * Gets the player and the UUID of the given chatter and adds them to the collection.
     * @param chatter the chatter that should be added to the list.
     * @return true if the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean addIgnoredChatter(@NotNull final IChatter chatter);

    /**
     * Adds a new player UUID to the list of all ignoring player UUIDs of this chatter.
     * @param playerUUID the UUID of the chatter that should be added to the list.
     * @return true if the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean addIgnoredChatter(@NotNull final UUID playerUUID);

    /**
     * Removes a player UUID from the list of all ignoring player UUIDs of this chatter.
     * Gets the player and the UUID of the given chatter and removes them from the collection.
     * @param chatter the chatter that should be removed from this list.
     * @return true if the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean removeIgnoredChatter(@NotNull final IChatter chatter);

    /**
     * Removes a player UUID from the list of all ignoring player UUIDs of this chatter.
     * @param playerUUID the UUID of the chatter that should be removed from this list.
     * @return true if the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean removeIgnoredChatter(@NotNull final UUID playerUUID);

    /**
     * Returns if this chatter is ignoring the given chatter.
     * Gets the player und the UUID of the given chatter and checks if this UUID is listed in the ignoring player
     * UUID list of this chatter.
     * @param chatter the chatter that should be checked.
     * @return true if this chatter is ignoring the given chatter.
     */
    boolean isIgnoring(@NotNull final IChatter chatter);

    /**
     * Returns if this chatter is ignoring a chatter with the given player UUID.
     * Checks if the given player UUID is listed in the ignoring player UUId list of this chatter.
     * @param playerUUID the player UUID that should be checked.
     * @return true if this chatter is ignoring a chatter with the given player UUID.
     */
    boolean isIgnoring(@NotNull final UUID playerUUID);
}