package de.g4memas0n.chats.storage;

import org.jetbrains.annotations.NotNull;
import java.io.File;

/**
 * Thrown to indicate that a file is a invalid storage file.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class InvalidStorageFileException extends Exception {

    private final File file;

    @SuppressWarnings("unused")
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

    @SuppressWarnings("unused")
    public InvalidStorageFileException(@NotNull final File file,
                                       @NotNull final String msg,
                                       @NotNull final Throwable cause) {
        super(msg, cause);
        this.file = file;
    }

    public @NotNull File getFile() {
        return this.file;
    }
}
