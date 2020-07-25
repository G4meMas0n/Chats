package de.g4memas0n.chats.messaging;

import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Chat Service Interface that provides method to get various chat options of players.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChat {

    /**
     * Returns the name of the given players primary group.
     *
     * @param player the player to get the primary group.
     * @return the name of the primary group or null when the given player has no group.
     */
    @Nullable String getGroup(@NotNull final Player player);

    /**
     * Returns the prefix of the given players primary group.
     *
     * <p>Returns an empty string when the given player has no primary group or when the group has no prefix.</p>
     *
     * @param player the player to get the prefix of the primary group.
     * @return the prefix of the given players primary group.
     */
    @NotNull String getGroupPrefix(@NotNull final Player player);

    /**
     * Returns the prefix of the given group in the given world.
     *
     * <p>Returns an empty string when the given group do not exist or when the given group has no prefix in the
     * given world.</p>
     *
     * @param world the world to check the groups prefix.
     * @param group the groups name to get the prefix.
     * @return the groups prefix in the given world.
     */
    @NotNull String getGroupPrefix(@NotNull final World world, @NotNull final String group);

    /**
     * Returns the suffix of the given players primary group.
     *
     * <p>Returns an empty string when the given player has no primary group or when the group has no suffix.</p>
     *
     * @param player the player to get the suffix of the primary group.
     * @return the suffix of the given players primary group.
     */
    @NotNull String getGroupSuffix(@NotNull final Player player);

    /**
     * Returns the suffix of the given group in the given world.
     *
     * <p>Returns an empty string when the given group do not exist or when the given group has no suffix in the
     * given world.</p>
     *
     * @param world the world to check the groups suffix.
     * @param group the groups name to get the suffix.
     * @return the groups suffix in the given world.
     */
    @NotNull String getGroupSuffix(@NotNull final World world, @NotNull final String group);

    /**
     * Returns the prefix of the given player.
     *
     * <p>Returns an empty string when the given player has no prefix.</p>
     *
     * @param player the player to get the prefix.
     * @return the players prefix.
     */
    @NotNull String getPlayerPrefix(@NotNull final Player player);

    /**
     * Returns the suffix of the given player.
     *
     * <p>Returns an empty string when the given player has no suffix.</p>
     *
     * @param player the player to get the suffix.
     * @return the players suffix.
     */
    @NotNull String getPlayerSuffix(@NotNull final Player player);

    /**
     * Returns the prefix for the given player.
     *
     * <p>When the given player has a prefix, the players prefix will be returned. Otherwise when the given player has
     * a primary group and this group has a prefix, the prefix of the players primary group will be returned.</p>
     *
     * @param player the player to get the prefix.
     * @return the prefix for the given player.
     */
    @SuppressWarnings("unused")
    @NotNull String getPrefix(@NotNull final Player player);

    /**
     * Returns the suffix for the given player.
     *
     * <p>When the given player has a suffix, the players suffix will be returned. Otherwise when the given player has
     * a primary group and this group has a suffix, the suffix of the players primary group will be returned.</p>
     *
     * @param player the player to get the suffix.
     * @return the suffix for the given player.
     */
    @SuppressWarnings("unused")
    @NotNull String getSuffix(@NotNull final Player player);
}
