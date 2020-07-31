package de.g4memas0n.chats.permission;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.storage.IStorageHolder;
import org.jetbrains.annotations.NotNull;

/**
 * Permissible Interface that provides methods to check whether a permissible is permitted to something or not.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IPermissible {

    /**
     * Checks whether this command source has the given permission node.
     *
     * @param node the permission node to check.
     * @return true when this command source has the permission, false otherwise.
     */
    boolean hasPermission(@NotNull final String node);

    /**
     * Returns whether this permissible is permitted to ban the given chatter from the given channel.
     *
     * @param chatter the chatter to check the permission.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canBan(@NotNull final IChatter chatter, @NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to create a channel of the given type.
     *
     * @param type the channel type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canCreate(@NotNull final IChannel.Type type);

    /**
     * Returns whether this permissible is permitted to delete the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canDelete(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to focus the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canFocus(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to ignore the given chatter.
     *
     * @param chatter the chatter to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canIgnore(@NotNull final IChatter chatter);

    /**
     * Returns whether this permissible is permitted to join the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canJoin(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to kick the given chatter from the given channel.
     *
     * @param chatter the chatter to check the permission.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canKick(@NotNull final IChatter chatter, @NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to leave the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canLeave(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to list the given channel type.
     *
     * @param type the channel type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canList(@NotNull final IChannel.Type type);

    /**
     * Returns whether this permissible is permitted to list the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canList(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to message the given chatter.
     *
     * <p>When this permissible can't see the given chatter it returns false.</p>
     *
     * @param chatter the chatter to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canMessage(@NotNull final IChatter chatter);

    /**
     * Returns whether this permissible is permitted to moderate the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModerate(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to modify the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModify(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to modify the given type of the given channel.
     *
     * @param channel the channel to check the permission.
     * @param modification the modification type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canModify(@NotNull final IChannel channel, @NotNull final IChannel.Modification modification);

    /**
     * Returns whether this permissible is permitted to mute the given chatter in the given channel.
     *
     * @param chatter the chatter to check the permission.
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canMute(@NotNull final IChatter chatter, @NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to reload the given storage-holder type.
     *
     * @param type the reload type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canReload(@NotNull final IStorageHolder.Type type);

    /**
     * Returns whether this permissible is permitted to save the given storage-holder type.
     *
     * @param type the type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canSave(@NotNull final IStorageHolder.Type type);

    /**
     * Returns whether this permissible can see the given chatter.
     *
     * @param chatter the chatter to check.
     * @return true when this permissible can see the given chatter, false otherwise.
     */
    boolean canSee(@NotNull final IChatter chatter);

    /**
     * Returns whether this permissible is permitted to speak in the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canSpeak(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to view the given info type of the given channel.
     *
     * @param channel the channel to check the permission.
     * @param information the information type to check the permission.
     * @return true when this permissible is permitted, false otherwise.
     */
    boolean canView(@NotNull final IChannel channel, @NotNull final IChannel.Information information);

    /**
     * Returns whether this permissible is permitted to view the information's of the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible it permitted, false otherwise.
     */
    boolean canViewInfo(@NotNull final IChannel channel);

    /**
     * Returns whether this permissible is permitted to view the members of the given channel.
     *
     * @param channel the channel to check the permission.
     * @return true when this permissible it permitted, false otherwise.
     */
    boolean canViewWho(@NotNull final IChannel channel);
}
