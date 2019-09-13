package de.g4memas0n.Chats.logger;

import de.g4memas0n.Chats.util.ANSIColor;
import org.jetbrains.annotations.NotNull;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * Formatter for the ChatLog files. Adds the exact time and strips all Colors from the record message.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 3rd, 2019
 * last change: September 13th, 2019
 */
public final class ChatLogFileFormatter extends Formatter {

    /**
     * the default date format. Used to format the exact time of a log that it is readable.
     */
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Override
    public @NotNull String format(LogRecord record) {
        return "[" + this.formatDate(record.getMillis()) + "] " + ANSIColor.stripColor(record.getMessage()) + "\n";
    }

    private @NotNull String formatDate(long milliSecs) {
        return DATE_FORMAT.format(new Date(milliSecs));
    }
}
