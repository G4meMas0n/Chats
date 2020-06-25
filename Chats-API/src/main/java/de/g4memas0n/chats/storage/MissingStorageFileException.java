package de.g4memas0n.chats.storage;

import org.jetbrains.annotations.NotNull;
import java.io.File;

/**
 * Thrown to indicate that a file of a storage file is missing.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: May 18th, 2020
 * changed: May 18th, 2020
 */
public final class MissingStorageFileException extends Exception {

    private final File file;

    @SuppressWarnings("unused")
    public MissingStorageFileException(@NotNull final File file,
                                       @NotNull final String msg) {
        super(msg);
        this.file = file;
    }

    public MissingStorageFileException(@NotNull final File file,
                                       @NotNull final Throwable cause) {
        super(cause);
        this.file = file;
    }

    @SuppressWarnings("unused")
    public MissingStorageFileException(@NotNull final File file,
                                       @NotNull final String msg,
                                       @NotNull final Throwable cause) {
        super(msg, cause);
        this.file = file;
    }

    public @NotNull File getFile() {
        return this.file;
    }
}
