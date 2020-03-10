package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.util.type.ModifyType;
import de.g4memas0n.Chats.util.type.ReloadType;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

/**
 * Permissible Interface that provides methods to check whether a permissible is permitted to something or not.
 * This Interface is only effectively used for the IChatter Interface, so a permissible is most of the time a chatter.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 8th, 2020
 * changed: March 9th, 2020
 */
public interface IPermissible {

    /**
     * Returns whether this permissible is permitted to broadcast to the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canBroadcast(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to create a channel of the given type.
     * @param type the channel type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canCreate(@NotNull final ChannelType type);

    /**
     * Returns whether this permissible is permitted to delete the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canDelete(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to focus the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canFocus(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to ignore the given player.
     * @param player the player to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canIgnore(@NotNull final Player player);

    /**
     * Returns whether this permissible is permitted to join the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canJoin(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to leave the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canLeave(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to list the given channel type.
     * @param type the channel type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canList(@NotNull final ChannelType type);

    /**
     * Returns whether this permissible is permitted to message the given player.
     * When this chatter can't see the given player it returns false.
     * @param player the player to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canMessage(@NotNull final Player player);

    /**
     * Returns whether this permissible is permitted to moderate the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModerate(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to modify the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModify(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to modify the given channel.
     * @param channel the channel to check the permission.
     * @param type the modify type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type);

    /**
     * Returns whether this permissible is permitted to reload the given reload type.
     * @param type the reload type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canReload(@NotNull final ReloadType type);

    /**
     * Returns whether this permissible is permitted to speak in the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canSpeak(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to view the information's of the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible it permitted, false otherwise.
     */
    boolean canViewInfo(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to view the members of the given channel.
     * @param channel the channel to check the permission.
     * @return true when this permissible it permitted, false otherwise.
     */
    boolean canViewWho(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is forced to focus the given channel on connect.
     * @param channel the channel to check the forced focus.
     * @return true when this permissible is forced to focus the given channel, false otherwise.
     */
    boolean forcedFocus(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is forced to join the given channel on connect.
     * @param channel the channel to check the forced join.
     * @return true when this permissible is forced to join the given channel, false otherwise.
     */
    boolean forcedJoin(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is forced to leave the given channel on disconnect.
     * @param channel the channel to check the forced leave.
     * @return true when this permissible is forced to leave the given channel, false otherwise.
     */
    boolean forcedLeave(@NotNull final IChannel channel);
}
