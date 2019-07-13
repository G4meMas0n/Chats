package de.g4memas0n.Chats.managers;

import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.storages.IChatterStorage;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;
import java.util.UUID;

public interface IChatterManager {

    /**
     * Returns the chatter storage representation for this chatter manager.
     * @return the chatter storage of this chatter manager.
     */
    @NotNull IChatterStorage getChatterStorage();

    /**
     * Returns if this chatter manager contains the given chatter.
     * @param chatter the chatter that should be checked.
     * @return true when the given chatter was found in this chatter manager.
     */
    boolean hasChatter(@NotNull final IChatter chatter);

    /**
     * Returns if this chatter manager contains a chatter with the given player.
     * @param player the player that should be checked.
     * @return true when a chatter with the given player was found in this chatter manager.
     */
    boolean hasChatter(@NotNull final Player player);

    /**
     * Returns if this chatter manager contains a chatter with the given player UUID.
     * @param playerUUID the player UUID that should be checked.
     * @return true when a chatter with the given player UUID was found in this chatter manager.
     */
    boolean hasChatter(@NotNull final UUID playerUUID);

    /**
     * Returns a collection of all chatters in this chatter manager.
     * @return all chatters of this chatter manager.
     */
    @NotNull Collection<IChatter> getChatters();

    /**
     * Returns the chatter with the given player.
     * @param player the player of the chatter.
     * @return the chatter with the given player or null if there is no chatter with the given player.
     */
    @Nullable IChatter getChatter(@NotNull final Player player);

    /**
     * Returns the chatter with the given player UUID.
     * @param playerUUID the player UUID of the chatter.
     * @return the chatter with the given player UUID or null if there is no chatter with the given player UUID.
     */
    @Nullable IChatter getChatter(@NotNull final String playerUUID);

    /**
     * Adds a new chatter to this chatter manager.
     * @param chatter the chatter that should be added.
     * @return true when the chatter was added as result of this call.
     */
    boolean addChatter(@NotNull final IChatter chatter);

    /**
     * Removes a chatter from this chatter manager.
     * Gets the player UUID of the given chatter and removes them from this chatter manager.
     * When the chatter is in a conversion channel, the conversion channels will be removed from the channel manager.
     * @param chatter the chatter that should be removed.
     * @return true when the chatter was removed as result of this call.
     */
    boolean removeChatter(@NotNull final IChatter chatter);

    /**
     * Removes a chatter from this chatter manager.
     * Gets the player UUID of the given player and removes them from this chatter manager.
     * When the chatter is in a conversion channel, the conversion channels will be removed from the channel manager.
     * @param player the player of the chatter that should be removed.
     * @return true when the chatter was removed as result of this call.
     */
    boolean removeChatter(@NotNull final Player player);

    /**
     * Removes a chatter from this chatter manager.
     * When the chatter is in a conversion channel, the conversion channels will be removed from the channel manager.
     * @param playerUUID the player UUID of the chatter that should be removed.
     * @return true when the chatter was removed as result of this call
     */
    boolean removeChatter(@NotNull final String playerUUID);
}