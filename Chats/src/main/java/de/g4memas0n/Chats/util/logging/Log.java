package de.g4memas0n.Chats.util.logging;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Logging class, that is used for all logging actions of this plugin.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 12th, 2020
 * changed: March 9th, 2020
 */
public final class Log {

    private static final SimpleDateFormat FILE_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private static final String DIRECTORY_LOGS = "logs";

    private static Logger pluginLogger;
    private static Logger chatLogger;

    private static boolean colored;
    private static boolean debug;

    private Log() { }

    public static @NotNull Logger getPluginLogger() {
        return pluginLogger;
    }

    public static void setPluginLogger(@NotNull final Logger logger) {
        pluginLogger = logger;
    }

    public static @NotNull Logger getChatLogger() {
        return chatLogger;
    }

    public static void setChatLogger(@NotNull final Logger logger) {
        chatLogger = logger;
    }

    public static boolean isColored() {
        return colored;
    }

    public static void setColored(final boolean enabled) {
        if (enabled == colored) {
            return;
        }

        colored = enabled;
    }

    public static boolean isDebug() {
        return debug;
    }

    public static void setDebug(final boolean enabled) {
        if (enabled == debug) {
            return;
        }

        debug = enabled;
    }

    /**
     * Setups a new File Handler and returns it. This method will use the given directory as parent directory to
     * create a logs folder, if it not exist, where the returning file handler saves the log files. Can be null, when
     * the file handler can not open the log file for any reason.
     * @param directory The parent directory for the log folder.
     * @return The created file handler or null when the file handler can not created for any reason.
     */
    public static @Nullable FileHandler setupFileHandler(@NotNull final File directory) {
        final File logFolder = new File(directory, DIRECTORY_LOGS);
        final String logPattern = logFolder.getAbsolutePath().replace("\\", "/") + "/"
                + FILE_DATE_FORMAT.format(new Date(System.currentTimeMillis())) + "%u.log";

        try {
            final FileHandler handler = new FileHandler(logPattern, true);

            handler.setFormatter(new LogFileFormatter());

            return handler;
        } catch (IOException ex) {
            return null;
        }
    }

    /**
     * Implements a {@link Formatter} that is used to format the chat log files.
     *
     * @author G4meMas0n
     * @since 0.1.0-SNAPSHOT
     *
     * created: February 12th, 2020
     * changed: March 6th, 2020
     */
    private static final class LogFileFormatter extends Formatter {

        private static final SimpleDateFormat LOG_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        private LogFileFormatter() {
            super();
        }

        @Override
        public @NotNull String format(@NotNull final LogRecord record) {
            return "[" + this.formatDate(record.getMillis()) + "] " + ANSIColor.stripColor(record.getMessage()) + "\n";
        }

        private @NotNull String formatDate(final long milliSecs) {
            return LOG_DATE_FORMAT.format(new Date(milliSecs));
        }
    }
}
