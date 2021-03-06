package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.storage.IStorageContainer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;

/**
 * Channel Manager Interface that defines a channel manager representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChannelManager extends IStorageContainer {

    // Default Channel Methods:
    /**
     * Returns the channel of this channel manager that is specified as a default channel.
     *
     * @return the specified default channel.
     */
    @NotNull IChannel getDefault();

    /**
     * Sets a new default channel for this channel manager when it is specified as a persist channel.
     *
     * <p><i><b>Note:</b> When the channel is not listed in this channel manager, this method will add them.</i></p>
     *
     * @param channel the new default channel for this channel manager.
     * @return true when the default channel of this manager was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the channel with the given name is not specified as persist channel.
     */
    @SuppressWarnings("unused")
    boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException;

    // Channel Collection Methods:
    /**
     * Returns all channels that are listed in this channel manager.
     *
     * @return a copy of the set of all listed channels in this channel manager.
     */
    @NotNull Set<IChannel> getChannels();

    /**
     * Returns a channel of this channel manager with the given full name.
     *
     * <p>When no channel with the given full name was found, it returns null.</p>
     *
     * @param fullName the name of the channel.
     * @return the channel with the given name or null when no channel with the given name was found.
     */
    @Nullable IChannel getChannel(@NotNull final String fullName);

    /**
     * Returns a persist channel of this channel manager with the given full name.
     *
     * <p>When no channel with the given full name was found or when the founded channel is not persistent, it
     * returns null.</p>
     *
     * @param fullName the name of the persist channel.
     * @return the persist channel with the given name or null when no persist channel with the given name was found.
     */
    @Nullable IChannel getPersist(@NotNull final String fullName);

    /**
     * Returns a standard channel of this manager with the given full name.
     *
     * <p>When no channel with the given full name was found or when the founded channel is not a standard channel, it
     * returns null.</p>
     *
     * @param fullName the name of the standard channel.
     * @return the standard channel with the given name or null when no standard channel with the given name was found.
     */
    @SuppressWarnings("unused")
    @Nullable IChannel getStandard(@NotNull final String fullName);

    /**
     * Returns a conversation channel for the two given chatters.
     *
     * <p>Returns a new created conversation channel for the given chatters when it does not already exist.</p>
     *
     * @param first the first chatter for the conversation.
     * @param second the second chatter for the conversation.
     * @return the conversation channel for the given chatters.
     */
    @NotNull IChannel getConversation(@NotNull final IChatter first,
                                      @NotNull final IChatter second);

    /**
     * Adds the given channel to this channel manager, when it is not already contained.
     *
     * @param channel the channel that should be added to this manager.
     * @return true when channel was added as result of this call, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes the given channel from this channel manager, when it is contained.
     *
     * @param channel the channel that should be removed from this manager.
     * @return true when channel was removed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given channel equals the default channel of this manager.
     */
    boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Returns whether this channel manager contains channels.
     *
     * @return true when this channel manager contains channels, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean hasChannels();

    /**
     * Returns whether this channel manager contains a channel with the given full name.
     *
     * @param fullName the full name of the channel that should be checked.
     * @return true when this channel manager contains a channel with the given full name, false otherwise.
     */
    boolean hasChannel(@NotNull final String fullName);
}
