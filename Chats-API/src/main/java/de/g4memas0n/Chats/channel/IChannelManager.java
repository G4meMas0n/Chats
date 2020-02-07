package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Set;

/**
 * Channel Manager Interface that defines a channel manager representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 4th, 2019
 * changed: February 1st, 2020
 */
public interface IChannelManager {

    /**
     * Returns the chat formatter of this channel manager, that is used for all channels.
     * @return the chat formatter.
     */
    @NotNull IChatFormatter getFormatter();

    /**
     * Returns the chat performer of this channel manager, that is used for all channels.
     * @return the chat performer.
     */
    @NotNull IChatPerformer getPerformer();

    // Default Channel Methods:
    /**
     * Returns the channel of this channel manager that is specified as a default channel.
     * @return the specified default channel.
     */
    @NotNull IChannel getDefault();

    /**
     * Sets a new default channel for this channel manager when it is specified as a persist channel.
     * When the channel is not listed in this channel manager, this method will add them.
     * @param channel the new default channel for this channel manager.
     * @return true when the default channel of this manager was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the channel with the given name is not specified as persist channel.
     */
    boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException;

    // Channel Collection Methods:
    /**
     * Returns all channels that are listed in this channel manager.
     * @return a copy of the set of all listed channels in this channel manager.
     */
    @NotNull Set<IChannel> getChannels();

    /**
     * Returns the channel of this channel manager with the given full name. Can be null when there is no channel with
     * the given full name.
     * @param fullName the full name of the channel that should be returned.
     * @return the channel with the given full name of this channel manager or null when there is no channel with the
     *         given full name.
     */
    @Nullable IChannel getChannel(@NotNull final String fullName);

    /**
     * Adds the given channel to this channel manager, when it is not already contained.
     * @param channel the channel that should be added to this manager.
     * @return true when channel was added as result of this call, false otherwise.
     */
    boolean addChannel(@NotNull final IChannel channel);

    /**
     * Removes the given channel from this channel manager, when it is contained.
     * @param channel the channel that should be removed from this manager.
     * @return true when channel was removed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given channel equals the default channel of this manager.
     */
    boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Returns whether this channel manager contains a channel with the given full name.
     * @param fullName the full name of the channel that should be checked.
     * @return true when this channel manager contains a channel with the given full name, false otherwise.
     */
    boolean hasChannel(@NotNull final String fullName);

    /**
     * Returns whether this channel manager contains a channel with the given full name that is specified as a persist
     * channel.
     * @param fullName the full name of the persist channel that should be checked.
     * @return true when this channel manager contains a persist channel with the given full name, false otherwise.
     */
    boolean hasPersist(@NotNull final String fullName);

    /**
     * Returns whether this channel manager contains a channel with the given full name that is specified as a
     * conversation channel.
     * @param fullName the full name of the conversion channel that should be checked.
     * @return true when this channel manager contains a conversation channel with the given full name, false otherwise.
     */
    boolean hasConversation(@NotNull final String fullName);

    /**
     * Create and returns a new channel that is not specified as a persist or conversation channel.
     * This method will not add the created channel to this channel manager.
     * @param fullName the full name for the channel that should be created.
     * @return the channel that was created.
     */
    @NotNull IChannel create(@NotNull final String fullName);

    /**
     * Create and returns a new channel that is specified as a persist channel.
     * This method will not add the created channel to this channel manager.
     * @param fullName the full name for the channel that should be created.
     * @return the channel that was created.
     */
    @NotNull IChannel createPersist(@NotNull final String fullName);

    /**
     * Create and returns a new channel that is specified as a conversation channel.
     * This method will not add the created channel to this channel manager.
     * @param first the first chatter that should be in the channel.
     * @param second the second chatter that should be in the channel.
     * @return the channel that was created.
     */
    @NotNull IChannel createConversation(@NotNull final IChatter first, @NotNull final IChatter second);

    /**
     * Reloads this channel manager. Removes all channels of this channel manager and loads all persist channels of the
     * persist channel directory.
     * This method will remove conversation and non persist channels entirely.
     * @throws IOException Thrown when the manager failed to load all persist channels.
     */
    void reload() throws IOException;
}
