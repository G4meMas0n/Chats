package de.g4memas0n.chats.util.logging;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.function.Supplier;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * The BasicLogger class that is a modified {@link java.util.logging.Logger} that offers a additional debug feature.
 * Also it provides the ability to prepend all logging calls with a specified prefix.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 24th, 2020
 * changed: June 15th, 2020
 */
public class BasicLogger extends Logger {

    private final String prefix;

    private FileHandler fileHandler;
    private boolean colored;
    private boolean debug;

    @SuppressWarnings("unused")
    public BasicLogger(@NotNull final String name) {
        super(name, null);

        this.fileHandler = null;
        this.prefix = "";
        this.colored = false;
        this.debug = false;
    }

    @SuppressWarnings("unused")
    public BasicLogger(@NotNull final String name, @NotNull final String prefix) {
        super(name, null);

        this.fileHandler = null;
        this.prefix = "[" + prefix + "] ";
        this.colored = false;
        this.debug = false;
    }

    public BasicLogger(@NotNull final Logger parent, @NotNull final String name) {
        super(parent.getName() + "_" + name, null);

        this.fileHandler = null;
        this.prefix = "";
        this.colored = false;
        this.debug = false;

        this.setParent(parent);
    }

    public BasicLogger(@NotNull final Logger parent, @NotNull final String name, @NotNull final String prefix) {
        super(parent.getName() + "_" + name, null);

        this.fileHandler = null;
        this.prefix = "[" + prefix + "] ";
        this.colored = false;
        this.debug = false;

        this.setParent(parent);
    }

    @SuppressWarnings("unused")
    public boolean isColored() {
        return this.colored;
    }

    public void setColored(final boolean colored) {
        if (this.colored == colored) {
            return;
        }

        this.colored = colored;
    }

    @SuppressWarnings("unused")
    public boolean isDebug() {
        return this.debug;
    }

    public void setDebug(final boolean debug) {
        if (this.debug == debug) {
            return;
        }

        this.debug = debug;
    }

    @Override
    public void log(@NotNull final LogRecord record) {
        if (this.colored) {
            record.setMessage(this.prefix + ANSICode.translateBukkitColor(record.getMessage()));
        } else {
            record.setMessage(this.prefix + ChatColor.stripColor(record.getMessage()));
        }

        super.log(record);
    }

    public void debug(@NotNull final String msg) {
        if (this.debug) {
            this.info(msg);
        }
    }

    @SuppressWarnings("unused")
    public void debug(@NotNull final Supplier<String> supplier) {
        if (this.debug) {
            this.info(supplier);
        }
    }

    public @Nullable FileHandler getFileHandler() {
        return this.fileHandler;
    }

    public void setFileHandler(@NotNull final FileHandler fileHandler) {
        if (fileHandler.equals(this.fileHandler)) {
            return;
        }

        this.fileHandler = fileHandler;
    }

    public boolean getUseFileHandler() {
        if (this.fileHandler != null) {
            for (final Handler current : this.getHandlers()) {
                if (current.equals(this.fileHandler)) {
                    return true;
                }
            }
        }

        return false;
    }

    public void setUseFileHandler(final boolean useFileHandler) {
        if (this.fileHandler == null || useFileHandler == this.getUseFileHandler()) {
            return;
        }

        if (useFileHandler) {
            this.addHandler(this.fileHandler);
        } else {
            this.removeHandler(this.fileHandler);
        }
    }
}
