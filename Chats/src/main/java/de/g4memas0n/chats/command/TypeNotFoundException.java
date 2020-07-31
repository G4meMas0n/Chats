package de.g4memas0n.chats.command;

import org.jetbrains.annotations.NotNull;

public final class TypeNotFoundException extends InvalidArgumentException {

    public TypeNotFoundException(@NotNull final String type) {
        super("typeNotFound", type);
    }

    public TypeNotFoundException(@NotNull final Throwable cause,
                                 @NotNull final String type) {
        super(cause, "typeNotFound", type);
    }
}
