package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.storage.IStorageContainer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Chatter Manager Interface that defines a chatter manager representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChatterManager extends IStorageContainer {

    /**
     * Returns all chatters that are listed in this chatter manager.
     *
     * @return the chatters in this chatter manager.
     */
    @NotNull Set<IChatter> getChatters();

    /**
     * Returns the chatter with the given name of this manager.
     *
     * <p>Can be null when there is no chatter with the given name in this manager.</p>
     *
     * @param name the name of the chatter that should be returned.
     * @return the chatter with the given name or null when there is no chatter with the given name.
     */
    @Nullable IChatter getChatter(@NotNull final String name);

    /**
     * Returns the chatter with the given uniqueId of this manager.
     *
     * <p>Can be null when there is no chatter with the given uniqueId in this manager.</p>
     *
     * @param uniqueId the uuid of the chatter that should be returned.
     * @return the chatter with the given uniqueId or null when there is no chatter with the given uniqueId.
     */
    @Nullable IChatter getChatter(@NotNull final UUID uniqueId);

    /**
     * Returns the chatter that represents the given player of this manager.
     *
     * <p>When there is no chatter that represents the given player in this manager then it will be loaded in this
     * manager.</p>
     *
     * @param player the player that should be represented by a chatter.
     * @return the chatter that represents the given player.
     */
    @NotNull IChatter getChatter(@NotNull final Player player);

    /**
     * Loads a new chatter that represents the given player, adds it to this manager and returns it.
     *
     * <p>When there is already a chatter that represents the given player in this manager then it returns this
     * chatter.</p>
     *
     * @param player the player that should be represented by a chatter.
     * @return the loaded chatter that represents the given player.
     */
    @NotNull IChatter loadChatter(@NotNull final Player player);

    /**
     * Unloads the chatter that represents the given player, removes it from this manager and returns it.
     *
     * <p><i><b>Note:</b> An unloaded chatter can no longer chat unless he gets loaded again.</i></p>
     *
     * @param player the player of the chatter to unload.
     * @return the unloaded chatter that represents the given player.
     */
    @NotNull IChatter unloadChatter(@NotNull final Player player);

    /**
     * Returns whether this chatter manager contains chatters.
     *
     * @return true when this chatter manager contains chatters, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean hasChatters();

    /**
     * Returns all offline chatters that exists in this chatter manager.
     *
     * @return the offline chatters in this chatter manager.
     */
    @NotNull Set<IOfflineChatter> getOfflineChatters();

    /**
     * Returns the offline chatter with the given name.
     *
     * <p>Can be null when there is no offline chatter with the given name in this manager.</p>
     *
     * @param name the name of the offline chatter.
     * @return the offline chatter with the given name or null if it does not exist.
     */
    @Nullable IOfflineChatter getOfflineChatter(@NotNull final String name);

    /**
     * Returns the offline chatter with the given uniqueId.
     *
     * <p>Can be null when there is no offline chatter with the given uniqueId in this manager.</p>
     *
     * @param uniqueId the uniqueId of the offline chatter.
     * @return the offline chatter with the given uniqueId or null if it does not exist.
     */
    @Nullable IOfflineChatter getOfflineChatter(@NotNull final UUID uniqueId);
}
