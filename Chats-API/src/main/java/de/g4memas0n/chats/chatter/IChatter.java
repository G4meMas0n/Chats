package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.IMessageRecipient;
import de.g4memas0n.chats.permission.IForcible;
import de.g4memas0n.chats.permission.IPermissible;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * IChatter Interface that defines a online chatter representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChatter extends IOfflineChatter, IMessageRecipient, IFilterable, IPermissible, IForcible, Comparable<IChatter> {

    /**
     * Returns the player of this chatter.
     *
     * @return the player of this chatter.
     */
    @NotNull Player getPlayer();

    /**
     * Returns the "friendly" name to display of this chatter.
     *
     * @return the display name of this chatter.
     * @see Player#getDisplayName()
     */
    @NotNull String getDisplayName();

    // Active Channel Methods:
    /**
     * Returns the currently channel in that this chatter is focused.
     *
     * @return the currently focused channel of this chatter.
     */
    @NotNull IChannel getFocus();

    /**
     * Sets a new channel as the currently focused channel of this chatter.
     *
     * <p>The channel can be null when the default channel should be the new focused channel.</p>
     *
     * <p><i><b>Note:</b> The old focused channel will be saved as 'last-focused-channel' when it is a non conversation
     * channel, and will be saved as 'last-persist-channel' when it is a persistent channel.</i></p>
     *
     * @param channel the new channel that should be the currently focused channel of this chatter, or null.
     * @return true when the currently focused channel was changed as result of this call, false otherwise.
     */
    boolean setFocus(@Nullable final IChannel channel);

    // Last Sources Methods:
    /**
     * Returns the last channel that this chatter had focused.
     *
     * <p>Can be null when there is no last focused channel.</p>
     *
     * <p><i><b>Note:</b> This Channel can be any non conversation channel.</i></p>
     *
     * @return the last focused channel of this chatter. or null when there is no last focused channel.
     */
    @SuppressWarnings("unused")
    @Nullable IChannel getLastFocused();

    /**
     * Returns the last persist channel that this chatter had focused.
     *
     * <p>Can be null when there is no last focused persist channel.</p>
     *
     * <p><i><b>Note:</b> This Channel can only be persist channels.</i></p>
     *
     * @return the last focused persist channel of this chatter or null when there is no last focused channel.
     */
    @SuppressWarnings("unused")
    @Nullable IChannel getLastPersist();

    /**
     * Returns the last partner this chatter had a conversation with.
     *
     * <p>Can be null when there is no last conversation this chatter had.</p>
     *
     * @return the last conversation partner.
     */
    @Nullable IChatter getLastPartner();

    /**
     * Sets a new chatters this chatter had a conversation with.
     *
     * @param partner the new last conversation partner.
     * @return true when the last conversation partner was changed as result of this call, false otherwise.
     */
    boolean setLastPartner(@NotNull final IChatter partner);

    // Channels Collection Methods:
    /**
     * Returns all channel that this chatter is in.
     *
     * <p>Can be all types of channels.</p>
     *
     * @return the channels this chatter is in.
     */
    @NotNull Set<IChannel> getChannels();

    /**
     * Joins a new channel.
     *
     * <p>This method will add this chatter to the given channel when it is not already added.</p>
     *
     * @param channel the channel to join.
     * @return true when the channel was joined as result of this call, false otherwise.
     * @see IChannel#addMember(IChatter) 
     */
    boolean joinChannel(@NotNull final IChannel channel);

    /**
     * Joins a new channel.
     *
     * <p>This method will add this chatter to the given channel when it is not already added.</p>
     *
     * @param channel the channel to join.
     * @param silent true when the channel should joined silently.
     * @return true when the channel was joined as result of this call, false otherwise.
     * @see IChannel#addMember(IChatter, boolean) 
     */
    boolean joinChannel(@NotNull final IChannel channel, final boolean silent);

    /**
     * Leaves a channel.
     *
     * <p>This method will remove this chatter from the given channel when it is not already removed.</p>
     *
     * @param channel the channel to leave.
     * @return true when the channel was left as result of this call, false otherwise.
     * @see IChannel#removeMember(IChatter)
     */
    boolean leaveChannel(@NotNull final IChannel channel);

    /**
     * Leaves a channel.
     *
     * <p>This method will remove this chatter from the given channel when it is not already removed.</p>
     *
     * @param channel the channel to leave.
     * @param silent true when the channel should left silently.
     * @return true when the channel was left as result of this call, false otherwise.
     * @see IChannel#removeMember(IChatter, boolean)
     */
    boolean leaveChannel(@NotNull final IChannel channel, final boolean silent);

    /**
     * Returns whether this chatter contains the given channel or not.
     *
     * @param channel the channel that should be checked.
     * @return true when the collection of channels contains given the channel, false otherwise.
     */
    boolean hasChannel(@NotNull final IChannel channel);

    // Ignored Chatter Collection Methods:
    /**
     * Adds a ignored player for this chatter.
     *
     * @param uniqueId the uniqueId of the player to add.
     * @return true when the player was ignored as result of this call, false otherwise.
     */
    boolean addIgnore(@NotNull final UUID uniqueId);

    /**
     * Removes a ignored player from this chatter.
     *
     * @param uniqueId the uniqueId of the player to remove.
     * @return true when the player was unignored as result of this call, false otherwise.
     */
    boolean removeIgnore(@NotNull final UUID uniqueId);

    /**
     * Returns whether the given uniqueId is ignored from this chatter.
     *
     * @param uniqueId the uniqueId of the player to check.
     * @return true when the given uniqueId is ignored, false otherwise.
     */
    boolean isIgnore(@NotNull final UUID uniqueId);

    /**
     * Returns whether this chatter is ignoring someone.
     *
     * @return true when someone is ignored, false otherwise.
     */
    boolean isIgnore();
}
