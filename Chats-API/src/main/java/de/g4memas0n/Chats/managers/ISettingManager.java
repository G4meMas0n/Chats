package de.g4memas0n.Chats.managers;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.storages.IConfigStorage;
import org.jetbrains.annotations.NotNull;
import java.util.Locale;

public interface ISettingManager {

    /**
     * Returns the config storage representation for this setting manager.
     * @return the config storage of this setting manager.
     */
    @NotNull IConfigStorage getConfigStorage();


    // Default Channel Methods:
    /**
     * Returns the default-channel option of the config.
     * @return the full name of the default channel.
     */
    @NotNull String getDefaultChannel();

    /**
     * Sets the default-channel option for the config.
     * @param channel the new default channel.
     * @return true if the option was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel isn't a global channel.
     */
    boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException;


    // Locale Methods:
    /**
     * Returns the locale option of the config.
     * @return the current locale.
     */
    @NotNull Locale getLocale();


    // Logs Methods:
    /**
     * Returns the log-chat-to-console option of the config.
     * @return true if this option is enabled.
     */
    boolean isLogChatToConsole();

    /**
     * Returns the log-chat-to-file option of the config.
     * @return true if this option is enabled.
     */
    boolean isLogChatToFile();

    /**
     * Sets the log-chat-to-file option for the config.
     * @param state if the chat should be logged into a file.
     * @return true if the option was changed as result of this call.
     */
    boolean setLogChatToFile(final boolean state);


    // Format Methods:
    /**
     * Returns the default-channel option of the config.
     * @return the default channel format.
     */
    @NotNull
    String getDefaultChannelFormat();

    /**
     * Returns the default-format-announce option of the config.
     * @return the default announce format.
     */
    @NotNull
    String getDefaultAnnounceFormat();

    /**
     * Returns the default-format-broadcast option of the config.
     * @return the default broadcast format.
     */
    @NotNull
    String getDefaultBroadcastFormat();

    /**
     * Returns the default-format-conversion option of the config.
     * @return the default conversion format.
     */
    @NotNull
    String getConversionFormat();
}