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
 * changed: February 8th, 2020
 */
public interface IChatterManager {

    /**
     * Returns all chatters that are listed in this chatter manager.
     * @return a copy of the collection of all listed chatters in this chatter manager.
     */
    @NotNull Set<IChatter> getChatters();

    /**
     * Returns the chatter with the given uniqueId of this manager. Can be null when there is no chatter with the given
     * uniqueId in this manager.
     * @param uniqueId the uuid of the chatter that should be returned.
     * @return the chatter with the given uniqueId or null when there is no chatter with the given uniqueId.
     */
    @Nullable IChatter getChatter(@NotNull final UUID uniqueId);

    /**
     * Returns the chatter that represents the given player of this manager. When there is no chatter that represents
     * the given player in this manager then it will be loaded in this manager.
     * @param player the player that should be represented by a chatter.
     * @return the chatter that represents the given player.
     */
    @NotNull IChatter getChatter(@NotNull final Player player);

    /**
     * Loads a new chatter that represents the given player, adds it to this manager and returns it.
     * When there is already a chatter that represents the given player in this manager then it returns this chatter.
     * @param player the player that should be represented by a chatter.
     * @return the loaded chatter that represents the given player.
     */
    @NotNull IChatter loadChatter(@NotNull final Player player);

    /**
     * Unloads the given chatter and removes it from this manager, when it is contained in this manager.
     * @param chatter the chatter that should be unloaded and removed from this manager.
     * @return true when chatter was unloaded and removed from this manager as result of this call, false otherwise.
     */
    boolean unloadChatter(@NotNull final IChatter chatter);

    /**
     * Returns whether this manager contains a chatter with the given uniqueId.
     * @param uniqueId the uniqueId of the chatter that should be checked.
     * @return true when this manager contains a chatter with the given uniqueId, false otherwise.
     */
    boolean hasChatter(@NotNull final UUID uniqueId);

    /**
     * Reloads this chatter manager.
     * This method will reload all chatters that are listed in this chatter manager.
     */
    void reload();
}
