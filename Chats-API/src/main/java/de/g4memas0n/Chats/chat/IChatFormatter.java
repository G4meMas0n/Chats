package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Placeholder;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ChatFormatter Interface that defines a formatter representation to format all chat messages.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: February 1st, 2020
 */
public interface IChatFormatter {

    /**
     * Returns the chat service registered by the vault plugin. Can be null when no chat service is registered.
     * @return the by vault registered chat service.
     */
    @Nullable Chat getChatService();

    /**
     * Sets the chat service registered by the vault plugin. Can be null to remove the chat service.
     * @param service the by vault registered chat service.
     * @return true when the chat service was changed as result of this call, false otherwise.
     */
    boolean setChatService(@Nullable final Chat service);

    /**
     * Returns the default announce format that was specified in the plugin configuration file.
     * @return the default announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the default announce format when it contain placeholder that must be included.
     * This Format must contain the {@link Placeholder#MESSAGE} Placeholder.
     * @param format the new default announce format.
     * @return true when the default announce format was changed as result of this call,
     *         false when the new format equals the old format.
     * @throws IllegalArgumentException Thrown when the given format do not contain the expected placeholders.
     */
    boolean setAnnounceFormat(@NotNull final String format) throws IllegalArgumentException;

    /**
     * Returns the default broadcast format that was specified in the plugin configuration file.
     * @return the default broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the default broadcast format when it contain placeholder that must be included.
     * This Format must contain the {@link Placeholder#BROADCAST_PREFIX} and {@link Placeholder#MESSAGE} Placeholder.
     * @param format the new default broadcast format.
     * @return true when the default broadcast format was changed as result of this call,
     *         false when the new format equals the old format.
     * @throws IllegalArgumentException Thrown when the given format do not contain the expected placeholders.
     */
    boolean setBroadcastFormat(@NotNull final String format) throws IllegalArgumentException;

    /**
     * Returns the default chat format that was specified in the plugin configuration file.
     * @return the default chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Sets the default chat format when it contain placeholder that must be included.
     * This Format must contain the {@link Placeholder#SENDER} or {@link Placeholder#SENDER_PLAIN} Placeholder and the
     * {@link Placeholder#MESSAGE} Placeholder.
     * @param format the new default chat format.
     * @return true when the default chat format was changed as result of this call,
     *         false when the new format equals the old format.
     * @throws IllegalArgumentException Thrown when the given format do not contain the expected placeholders.
     */
    boolean setChatFormat(@NotNull final String format) throws IllegalArgumentException;

    /**
     * Returns the default chat color that was specified in the plugin configuration file.
     * @return the default chat color.
     */
    @NotNull ChatColor getChatColor();

    /**
     * Sets the default chat color for the chat.
     * @param color the new default chat color.
     * @return true when the default chat color was changed as result of this call,
     *         false when the new color equals the old color.
     */
    boolean setChatColor(@NotNull final ChatColor color);

    /**
     * Returns the conversation chat format that was specified in the plugin configuration file.
     * @return the conversation chat format.
     */
    @NotNull String getConversationFormat();

    /**
     * Sets the conversation chat format when it contain placeholder that must be included.
     * This Format must contain the {@link Placeholder#MESSAGE}, {@link Placeholder#CON_PARTNER} and
     * {@link Placeholder#SENDER} or {@link Placeholder#CON_ADDRESS} Placeholder.
     * @param format the new conversation chat format.
     * @return true when the conversation chat format was changed as result of this call,
     *         false when the new format equals the old format.
     * @throws IllegalArgumentException Thrown when the given format do not contain the expected placeholders.
     */
    boolean setConversationFormat(@NotNull final String format) throws IllegalArgumentException;

    /**
     * Returns the default conversation color that was specified in the plugin configuration file.
     * @return the default conversation color.
     */
    @NotNull ChatColor getConversationColor();

    /**
     * Sets the default conversation color for all conversations.
     * @param color the new default conversation color.
     * @return true when the default conversation color was changed as result of this call,
     *         false when the new color equals the old color.
     */
    boolean setConversationColor(@NotNull final ChatColor color);

    /**
     * Formats the given announce format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format.
     * @param channel the channel that need to format this announce message.
     * @param format the format that is used for this announce message.
     * @param message the message for this announce.
     * @return the formatted announce message without any Placeholder.
     */
    @NotNull String formatAnnounce(@NotNull final IChannel channel,
                                   @NotNull final String format,
                                   @NotNull final String message);

    /**
     * Formats the given broadcast format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format.
     * @param channel the channel that need to format this broadcast message.
     * @param format the format that is used for this broadcast message.
     * @param message the message for this broadcast.
     * @return the formatted broadcast message without any Placeholder.
     */
    @NotNull String formatBroadcast(@NotNull final IChannel channel,
                                    @NotNull final String format,
                                    @NotNull final String message);

    /**
     * Formats the given chat format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format.
     * @param channel the channel that need to format this chat message.
     * @param format the format that is used for this chat message.
     * @param sender the sender of the given message.
     * @param message the message from the sender.
     * @return the formatted chat message without any Placeholder.
     */
    @NotNull String formatChat(@NotNull final IChannel channel,
                               @NotNull final String format,
                               @NotNull final IChatter sender,
                               @NotNull final String message);

    /**
     * Formats the given conversation format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format. This method will strip the {@link Placeholder#CON_ADDRESS} placeholder.
     * @param format the format that is used for this conversation message.
     * @param sender the sender of the given message.
     * @param partner the partner that should receive the given message from the sender.
     * @param message the message from the sender.
     * @return the formatted conversation message without any Placeholder.
     */
    @NotNull String formatConversation(@NotNull final String format,
                                       @NotNull final IChatter sender,
                                       @NotNull final IChatter partner,
                                       @NotNull final String message);

    /**
     * Formats the given conversation format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format. This method will strip the {@link Placeholder#CON_SENDER} placeholder.
     * @param format the format that is used for this conversation message.
     * @param address the address for this conversation message.
     * @param partner the partner of the conversation.
     * @param message the message of the conversation.
     * @return the formatted conversation message without any Placeholder.
     */
    @NotNull String formatAddress(@NotNull final String format,
                                  @NotNull final String address,
                                  @NotNull final IChatter partner,
                                  @NotNull final String message);
}
