package de.g4memas0n.Chats.storages;

import org.jetbrains.annotations.NotNull;
import java.io.File;

/**
 * Thrown to indicate that a file is a invalid storage file.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 3rd, 2019
 * last change: September 13th, 2019
 */
public final class InvalidStorageFileException extends Exception {
    private final File file;

    public InvalidStorageFileException(@NotNull final File file) {
        this.file = file;
    }

    public InvalidStorageFileException(@NotNull final File file,
                                       @NotNull final String msg) {
        super(msg);
        this.file = file;
    }

    public InvalidStorageFileException(@NotNull final File file,
                                       @NotNull final Throwable cause) {
        super(cause);
        this.file = file;
    }

    public InvalidStorageFileException(@NotNull final File file,
                                       @NotNull final String msg,
                                       @NotNull final Throwable cause) {
        super(msg, cause);
        this.file = file;
    }

    public @NotNull File getExceptionFile() {
        return this.file;
    }
}
