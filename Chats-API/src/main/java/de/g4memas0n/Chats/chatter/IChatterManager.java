package de.g4memas0n.Chats.chatter;

import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Chatter Manager Interface that defines a chatter manager representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 4th, 2019
 * last change: November 14th, 2019
 */
public interface IChatterManager {

    /**
     * Returns the chatter storage representation for this chatter manager.
     * @return the chatter storage of this chatter manager.
     */
    @NotNull IChatterStorage getChatterStorage();

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
     * Returns a set of all chatters in this chatter manager.
     * @return all chatters of this chatter manager.
     */
    @NotNull Set<IChatter> getChatters();

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
    @Nullable IChatter getChatter(@NotNull final UUID playerUUID);

    /**
     * Loads a new chatter and add it to this chatter manager.
     * @param player the player of the chatter that should be loaded.
     * @return true when the chatter was loaded and added as result of this call.
     */
    boolean loadChatter(@NotNull final Player player);

    /**
     * Unloads and removes a chatter from this chatter manager.
     * Gets the player UUID of the given player and unload and removes them from this chatter manager.
     * When the chatter is in a conversion channel, the conversion channels will be removed from the channel manager.
     * @param player the player of the chatter that should be unloaded and removed.
     * @return true when the chatter was unloaded and removed as result of this call.
     */
    boolean unloadChatter(@NotNull final Player player);

    /**
     * Unloads and removes a chatter from this chatter manager.
     * When the chatter is in a conversion channel, the conversion channels will be removed from the channel manager.
     * @param playerUUID the player UUID of the chatter that should be unloaded and removed.
     * @return true when the chatter was unloaded and removed as result of this call
     */
    boolean unloadChatter(@NotNull final UUID playerUUID);
}
