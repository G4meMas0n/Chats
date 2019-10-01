package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Chatter Interface that defines a chatter representation.
 * Extends the Comparable interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 12th, 2019
 * last change: October 1st, 2019
 */
public interface IChatter extends Comparable<IChatter> {

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
     * Sets the old active channel as last persist channel if it was a persist channel.
     * @param channel the new active channel for this chatter.
     * @return true when the active channel was changed as result of this call.
     */
    boolean setActiveChannel(@NotNull final IChannel channel);

    // Last Sources Methods:
    /**
     * Returns the last active global channel of this chatter.
     * @return the last global channel of this chatter or null if this chatter don't changed the channel.
     */
    @Nullable IChannel getLastPersistChannel();

    /**
     * Returns the last conversion partner of this chatter.
     * @return the chatters of the last conversion or null if this chatter don't have at least one last conversion
     *         partner.
     */
    @Nullable Set<IChatter> getLastConversionPartner();

    /**
     * Sets the last conversion partner of this chatter.
     * When this given chatters contains this chatter, it will be removed from the set.
     * @param chatters the last conversion partner.
     * @return true when the last conversion partner was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the set of given chatters is empty.
     */
    boolean setLastConversionPartner(@NotNull final Set<IChatter> chatters) throws IllegalArgumentException;

    // Channels Collection Methods:
    /**
     * Returns the set of all channels this chatter is in.
     * @return all channels this chatter is in.
     */
    @NotNull Set<IChannel> getChannels();

    /**
     * Adds a new channel to this chatter and also adds this chatter to the chatter list of the given channel.
     * @param channel the new channel that should be added to this chatter.
     * @return true when the channel collection of this chatter and the chatter list of the given channel was
     *              changed as result of this call.
     */
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes a channel from this chatter and also removes this chatter from the chatter list of the given channel.
     * @param channel the channel that should be removed from this chatter.
     * @return true when the channel collection of this chatter and the chatter list of the given channel was
     *              changed as result of this call.
     */
    boolean removeChannel(@NotNull final IChannel channel);

    // Ignored Chatter Collection Methods:
    /**
     * Returns the set of all player UUIDs that this chatter is ignoring.
     * @return all ignored player UUIDs of this chatter.
     */
    @NotNull Set<UUID> getIgnores();

    /**
     * Adds a new player UUID to the collection of all ignoring player UUIDs of this chatter.
     * Gets the player and the UUID of the given chatter and adds them to the collection.
     * @param chatter the chatter that should be added to the collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean addIgnores(@NotNull final IChatter chatter);

    /**
     * Adds a new player UUID to the collection of all ignoring player UUIDs of this chatter.
     * Gets the UUID of the given player and adds them to the collection.
     * @param player the player that should be added to the collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean addIgnores(@NotNull final Player player);

    /**
     * Adds a new player UUID to the collection of all ignoring player UUIDs of this chatter.
     * @param playerUUID the UUID of the chatter that should be added to the collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean addIgnores(@NotNull final UUID playerUUID);

    /**
     * Removes a player UUID from the collection of all ignoring player UUIDs of this chatter.
     * Gets the player and the UUID of the given chatter and removes them from the collection.
     * @param chatter the chatter that should be removed from this collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean removeIgnores(@NotNull final IChatter chatter);

    /**
     * Removes a player UUID from the collection of all ignoring player UUIDs of this chatter.
     * Gets the UUID of the given player and removes them to the collection.
     * @param player the player that should be removed from the collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean removeIgnores(@NotNull final Player player);

    /**
     * Removes a player UUID from the collection of all ignoring player UUIDs of this chatter.
     * @param playerUUID the UUID of the chatter that should be removed from this collection.
     * @return true when the list of all ignored player UUIDs was changed as result of this call.
     */
    boolean removeIgnores(@NotNull final UUID playerUUID);

    /**
     * Returns if this chatter is ignoring the given chatter.
     * Gets the player und the UUID of the given chatter and checks if this UUID is listed in the ignoring player
     * UUID collection of this chatter.
     * @param chatter the chatter that should be checked.
     * @return true when this chatter is ignoring the given chatter.
     */
    boolean isIgnoring(@NotNull final IChatter chatter);

    /**
     * Returns if this chatter is ignoring the given Player.
     * Gets the UUID of the given Player and checks if this UUID is listed in the ignoring player UUID collection of
     * this chatter.
     * @param player the player that should be checked.
     * @return true when this chatter is ignoring the given UUID.
     */
    boolean isIgnoring(@NotNull final Player player);

    /**
     * Returns if this chatter is ignoring a chatter with the given player UUID.
     * Checks if the given player UUID is listed in the ignoring player UUId list of this chatter.
     * @param playerUUID the player UUID that should be checked.
     * @return true when this chatter is ignoring a chatter with the given player UUID.
     */
    boolean isIgnoring(@NotNull final UUID playerUUID);
}
