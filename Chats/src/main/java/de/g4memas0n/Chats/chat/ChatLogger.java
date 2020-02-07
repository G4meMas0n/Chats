package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.util.ANSIColor;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * The ChatLogger class that is a modified {@link java.util.logging.Logger} that prepends all logging calls with the
 * prefix and translates or strips color codes included in all log record messages.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: January 6th, 2020
 */
public final class ChatLogger extends Logger {

    private static final SimpleDateFormat FILE_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private static final SimpleDateFormat LOG_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    private static final String DIRECTORY_LOGS = "logs";

    private final String prefix;
    private boolean colored;

    public ChatLogger(@NotNull final Logger parent, final boolean logColored) {
        super(parent.getName() + "_Chat", null);

        this.prefix = "[Chat]";
        this.colored = logColored;

        this.setParent(parent);
        this.setLevel(Level.ALL);
    }

    @Override
    public void log(@NotNull final LogRecord record) {
        if (colored) {
            record.setMessage(this.prefix + ANSIColor.translateBukkitColor(record.getMessage()));
        } else {
            record.setMessage(this.prefix + ChatColor.stripColor(record.getMessage()));
        }

        super.log(record);
    }

    public static @NotNull FileHandler getFileHandler(@NotNull final File dataFolder) throws IllegalArgumentException, IOException {
        if (!dataFolder.isDirectory()) {
            throw new IllegalArgumentException("Invalid file! file must be a directory");
        }

        final File directory = new File(dataFolder, DIRECTORY_LOGS);

        if (!directory.exists()) {
            directory.mkdirs();
        }

        final Date crtTime = new Date(System.currentTimeMillis());

        // replace to 'logFolder.getAbsolutePath().replace("\\", "/")' when the current solution do not work.
        final String logPattern = directory.getAbsolutePath() + "/" + FILE_DATE_FORMAT.format(crtTime) + "%u.log";

        final FileHandler fileHandler = new FileHandler(logPattern, true);

        fileHandler.setFormatter(new FileFormatter());

        return fileHandler;
    }

    /**
     * Implements a formatter that is used to format the chat log files.
     */
    public static final class FileFormatter extends Formatter {

        @Override
        public @NotNull String format(LogRecord record) {
            return "[" + this.formatDate(record.getMillis()) + "] " + ANSIColor.stripColor(record.getMessage()) + "\n";
        }

        private @NotNull String formatDate(long milliSecs) {
            return LOG_DATE_FORMAT.format(new Date(milliSecs));
        }
    }
}
