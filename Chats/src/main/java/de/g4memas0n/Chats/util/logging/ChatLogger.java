package de.g4memas0n.Chats.util.logging;

import org.jetbrains.annotations.NotNull;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * The ChatLogger class that is a modified {@link java.util.logging.Logger} that prepends all logging calls with the
 * chat prefix.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 2nd, 2020
 * changed: February 12th, 2020
 */
public final class ChatLogger extends Logger {

    private final String prefix;

    public ChatLogger(@NotNull final Logger parent) {
        super(parent.getName() + "_Chat", null);

        this.prefix = "[Chat]";

        this.setParent(parent);
        this.setLevel(Level.ALL);
    }

    @Override
    public void log(@NotNull final LogRecord record) {
        record.setMessage(this.prefix + record.getMessage());

        super.log(record);
    }
}
