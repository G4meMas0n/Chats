package de.g4memas0n.Chats.logger;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

public final class ChatLogFileFormatter extends Formatter {
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Override
    public String format(LogRecord record) {
        return "[" + this.formatDate(record.getMillis()) + "] " + record.getMessage() + "\n";
    }

    private String formatDate(long milliSecs) {
        return DATE_FORMAT.format(new Date(milliSecs));
    }
}