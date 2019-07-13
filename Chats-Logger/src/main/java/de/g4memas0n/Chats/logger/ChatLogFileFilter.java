package de.g4memas0n.Chats.logger;

import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public final class ChatLogFileFilter implements Filter {

    @Override
    public boolean isLoggable(LogRecord record) {
        return record.getLevel().equals(Level.FINE);
    }
}
