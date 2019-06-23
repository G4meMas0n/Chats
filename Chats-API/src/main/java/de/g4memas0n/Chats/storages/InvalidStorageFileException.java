package de.g4memas0n.Chats.storages;

import org.jetbrains.annotations.NotNull;

public class InvalidStorageFileException extends Exception {

    public InvalidStorageFileException() {
    }

    public InvalidStorageFileException(@NotNull final String msg) {
        super(msg);
    }

    public InvalidStorageFileException(@NotNull final Throwable cause) {
        super(cause);
    }

    public InvalidStorageFileException(@NotNull final String msg, @NotNull final Throwable cause) {
        super(msg, cause);
    }
}
