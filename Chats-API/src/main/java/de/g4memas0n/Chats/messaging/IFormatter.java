package de.g4memas0n.Chats.messaging;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;

/**
 * Formatter Interface that defines a formatter representation to format all chat messages and to get default
 * formatting values.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: March 4th, 2020
 */
public interface IFormatter {

    /**
     * Returns the default announce format that was specified in the plugin configuration file.
     * @return the default announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Returns the default broadcast format that was specified in the plugin configuration file.
     * @return the default broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Returns the default chat format that was specified in the plugin configuration file.
     * @return the default chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Returns the conversation chat format that was specified in the plugin configuration file.
     * @return the conversation chat format.
     */
    @NotNull String getConversationFormat();

    /**
     * Returns the default channel color that was specified in the plugin configuration file.
     * @return the default channel color.
     */
    @NotNull ChatColor getChannelColor();

    /**
     * Returns the default conversation color that was specified in the plugin configuration file.
     * @return the default conversation color.
     */
    @NotNull ChatColor getConversationColor();

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
