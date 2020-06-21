package de.g4memas0n.chats.storage.configuration;

import de.g4memas0n.chats.storage.IStorageHolder;
import org.jetbrains.annotations.NotNull;
import java.util.Locale;

/**
 * Settings Interface that defines a representation for the plugins config file.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 12th, 2019
 * changed: June 1st, 2020
 */
public interface ISettings extends IStorageHolder {

    /**
     * Returns the configured announce format from the config file.
     * @return the announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Returns the configured broadcast format from the config file.
     * @return the broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Returns the configured chat format from the config file.
     * @return the chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Returns the configured conversation format from the config file.
     * @return the conversation format.
     */
    @NotNull String getConversationFormat();

    /**
     * Returns the configured default channel name from the config file.
     * @return the default channel name.
     */
    @NotNull String getDefaultChannel();

    /**
     * Returns the configured locale from the config file.
     * @return the locale
     */
    @NotNull Locale getLocale();

    /**
     * Returns the configured state whether the chat log should be colored or not.
     * @return true when the chat log should be colored, false otherwise.
     */
    boolean isLogColored();

    /**
     * Returns the configured state whether debug messages should be logged or not.
     * @return true when debug messages should be logged, false otherwise.
     */
    boolean isLogDebug();

    /**
     * Returns the configured state whether the chat log should be logged to the console or not.
     * @return true when the chat log should be logged to console, false otherwise.
     */
    boolean isLogToConsole();

    /**
     * Returns the configured state whether the chat log should be logged to a file or not.
     * @return true when the chat log should be logged to file, false otherwise.
     */
    boolean isLogToFile();

    /**
     * Returns the configured state whether the storage holders should auto-save or not.
     * @return true when the storage holders should auto-save, false otherwise.
     */
    boolean isAutoSave();

    /**
     * Returns the configured auto save delay from the config file.
     * @return the auto save delay.
     */
    int getSaveDelay();
}
