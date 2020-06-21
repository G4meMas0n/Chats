package de.g4memas0n.chats.messaging;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;

/**
 * Formatter Interface that defines a formatter representation to format all chat messages and to get default
 * formatting values.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: May 1st, 2020
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
     * Formats the given announce format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format.
     * @param channel the channel to format this announce message.
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
     * @param channel the channel to format this broadcast message.
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
     * @param channel the channel to format this chat message.
     * @param format the format that is used for this chat message.
     * @param sender the sender of the message.
     * @param message the message from the sender.
     * @return the formatted chat message without any Placeholders.
     */
    @NotNull String formatChat(@NotNull final IChannel channel,
                               @NotNull final String format,
                               @NotNull final IChatter sender,
                               @NotNull final String message);

    /**
     * Formats the given conversation format. Replaces all Placeholder that are available for this format and strips all
     * other Placeholder from the given format. This method will replace the {@link Placeholder#CON_ADDRESS} placeholder
     * to "{0}" and the {@link Placeholder#CON_PARTNER} placeholder to "{1}".
     * @param channel the conversation channel to format this conversation message.
     * @param format the format that is used for the conversation message.
     * @param message the message from the sender.
     * @return the formatted conversation message without any Placeholders.
     */
    @NotNull String formatConversation(@NotNull final IChannel channel,
                                       @NotNull final String format,
                                       @NotNull final String message);
}
