package de.g4memas0n.chats.util.logging;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.storage.configuration.ISettings;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Logging class, that is used to access the plugin and chat logger.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 12th, 2020
 * changed: June 15th, 2020
 */
public final class Log {

    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private static final String DIRECTORY_LOGS = "logs";

    private static BasicLogger pluginLogger;
    private static BasicLogger chatLogger;

    private Log() { }

    public static @NotNull BasicLogger getPlugin() {
        if (pluginLogger == null) {
            throw new IllegalStateException("Tried to get plugin logger before it was set");
        }

        return pluginLogger;
    }

    public static @NotNull BasicLogger getChat() {
        if (chatLogger == null) {
            throw new IllegalStateException("Tried to get plugin logger before it was set");
        }

        return chatLogger;
    }

    public static void initialize(@NotNull final Logger parent, @NotNull final IChats instance) {
        if (pluginLogger != null && chatLogger != null) {
            pluginLogger.severe("Tried to initialize loggers twice. They are already initialized.");
            return;
        }

        pluginLogger = new BasicLogger(parent, "Plugin", instance.getDescription().getName());
        chatLogger = new BasicLogger(parent, "Chat");

        try {
            final File directory = new File(instance.getDataFolder(), DIRECTORY_LOGS);

            if (directory.mkdirs()) {
                pluginLogger.debug(String.format("Directory '%s' does not exist. Creating it...", directory));
            }

            final String pattern = directory.getAbsolutePath() + "/" + DATE_FORMAT.format(new Date(System.currentTimeMillis())) + "_Chat-%u.log";
            final FileHandler handler = new FileHandler(pattern, true);

            handler.setFormatter(new FileFormatter());

            chatLogger.setFileHandler(handler);
        } catch (IOException ex) {
            pluginLogger.warning(String.format("Unable to create file handler: %s", ex.getMessage()));
        }
    }

    public static void load(@NotNull final ISettings settings) {
        if (pluginLogger != null) {
            pluginLogger.setDebug(settings.isLogDebug());
        }

        if (chatLogger != null) {
            chatLogger.setColored(settings.isLogColored());
            chatLogger.setUseParentHandlers(settings.isLogToConsole());
            chatLogger.setUseFileHandler(settings.isLogToFile());
        }
    }

    public static void exit() {
        if (pluginLogger != null) {
            pluginLogger = null;
        }

        if (chatLogger != null) {
            if (chatLogger.getFileHandler() != null) {
                chatLogger.getFileHandler().close();
            }

            chatLogger = null;
        }
    }

    /**
     * Implements a {@link Formatter} that is used for the file handler of this logger.
     */
    private static final class FileFormatter extends Formatter {

        private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        private FileFormatter() {
            super();
        }

        @Override
        public @NotNull String format(@NotNull final LogRecord record) {
            return "[" + this.formatDate(record.getMillis()) + "]" + this.formatMessage(record.getMessage()) + "\n";
        }

        private @NotNull String formatDate(final long milliSecs) {
            return DATE_FORMAT.format(new Date(milliSecs));
        }

        private @NotNull String formatMessage(@NotNull final String message) {
            return ANSICode.stripColor(message);
        }
    }
}
