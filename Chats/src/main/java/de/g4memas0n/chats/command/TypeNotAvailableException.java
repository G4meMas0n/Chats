package de.g4memas0n.chats.command;

import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.IType;
import org.jetbrains.annotations.NotNull;

public final class TypeNotAvailableException extends InvalidArgumentException {

    public TypeNotAvailableException(@NotNull final IType type) {
        super("typeNotAvailable", Messages.tl(type.getKey()));
    }

    public TypeNotAvailableException(@NotNull final Throwable cause,
                                     @NotNull final IType type) {
        super(cause, "typeNotAvailable", Messages.tl(type.getKey()));
    }
}
