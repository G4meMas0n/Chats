package de.g4memas0n.chats.storage.configuration;

import de.g4memas0n.chats.storage.IStorageHolder;
import org.jetbrains.annotations.NotNull;
import java.util.Locale;

/**
 * Settings Interface that defines a representation for the plugins config file.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface ISettings extends IStorageHolder {

    /**
     * The default value of the announce format setting.
     */
    @NotNull String ANNOUNCE_FORMAT = "{color}{message}";

    /**
     * Returns the configured announce format from the config file.
     *
     * @return the announce format.
     * @see ISettings#ANNOUNCE_FORMAT
     */
    @NotNull String getAnnounceFormat();

    /**
     * The default value of the broadcast format setting.
     */
    @NotNull String BROADCAST_FORMAT = "{color}[{nick}][§aBroadcast{color}] {message}";

    /**
     * Returns the configured broadcast format from the config file.
     *
     * @return the broadcast format.
     * @see ISettings#BROADCAST_FORMAT
     */
    @NotNull String getBroadcastFormat();

    /**
     * The default value of the chat format setting.
     */
    @NotNull String CHAT_FORMAT = "{color}[{nick}]§r{sender}{color}: {message}";

    /**
     * Returns the configured chat format from the config file.
     *
     * @return the chat format.
     * @see ISettings#CHAT_FORMAT
     */
    @NotNull String getChatFormat();

    /**
     * The default value of the conversation format setting.
     */
    @NotNull String CONVERSATION_FORMAT = "{color}{con-address} {con-partner}{color}: {message}";

    /**
     * Returns the configured conversation format from the config file.
     *
     * @return the conversation format.
     * @see ISettings#CONVERSATION_FORMAT
     */
    @NotNull String getConversationFormat();

    /**
     * The default value of the default channel setting.
     */
    @NotNull String DEFAULT_CHANNEL = "Global";

    /**
     * Returns the configured default channel name from the config file.
     *
     * @return the default channel name.
     * @see ISettings#DEFAULT_CHANNEL
     */
    @NotNull String getDefaultChannel();

    /**
     * The default value of the locale setting.
     */
    @NotNull Locale LOCALE = Locale.ENGLISH;

    /**
     * Returns the configured locale from the config file.
     *
     * @return the locale
     * @see ISettings#LOCALE
     */
    @NotNull Locale getLocale();

    /**
     * The default value of the log colored setting.
     */
    boolean LOG_COLORED = true;

    /**
     * Returns the configured state whether the chat log should be colored or not.
     *
     * @return true when the chat log should be colored, false otherwise.
     * @see ISettings#LOG_COLORED
     */
    boolean isLogColored();

    /**
     * The default value of the log debug setting.
     */
    boolean LOG_DEBUG = false;

    /**
     * Returns the configured state whether debug messages should be logged or not.
     *
     * @return true when debug messages should be logged, false otherwise.
     * @see ISettings#LOG_DEBUG
     */
    boolean isLogDebug();

    /**
     * The default value of the log to console setting.
     */
    boolean LOG_TO_CONSOLE = true;

    /**
     * Returns the configured state whether the chat log should be logged to the console or not.
     *
     * @return true when the chat log should be logged to console, false otherwise.
     * @see ISettings#LOG_TO_CONSOLE
     */
    boolean isLogToConsole();

    /**
     * The default value of the log to file setting.
     */
    boolean LOG_TO_FILE = true;

    /**
     * Returns the configured state whether the chat log should be logged to a file or not.
     *
     * @return true when the chat log should be logged to file, false otherwise.
     * @see ISettings#LOG_TO_FILE
     */
    boolean isLogToFile();

    /**
     * The default value of the auto save setting.
     */
    boolean AUTO_SAVE = true;

    /**
     * Returns the configured state whether the storage holders should auto-save or not.
     *
     * @return true when the storage holders should auto-save, false otherwise.
     * @see ISettings#AUTO_SAVE
     */
    boolean isAutoSave();

    /**
     * The default value of the auto save delay setting.
     */
    long AUTO_SAVE_DELAY = 60;

    /**
     * Returns the configured auto save delay from the config file.
     *
     * @return the auto save delay.
     * @see ISettings#AUTO_SAVE_DELAY
     */
    long getSaveDelay();

    /**
     * The default value of the inform setting.
     */
    boolean INFORM = true;

    /**
     * Returns the configured state whether a chatter should be informed about the current focused channel on join.
     *
     * @return true when the chatter should be informed about the current focused channel.
     * @see ISettings#INFORM
     */
    boolean isInform();

    /**
     * The default value of the inform delay setting.
     */
    long INFORM_DELAY = 0;

    /**
     * Returns the configured inform delay from the config file.
     *
     * @return the inform delay.
     * @see ISettings#INFORM_DELAY
     */
    long getInformDelay();

    /**
     * The default value of the create limit setting.
     */
    int CREATE_LIMIT = 1;

    /**
     * Returns the configured create limit from the config file.
     *
     * <p><i><b>Note:</b> This option is currently a hidden option in the configuration file.</i></p>
     *
     * @return the creation limit.
     * @see ISettings#CREATE_LIMIT
     */
    int getCreateLimit();
}
