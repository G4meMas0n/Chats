package de.g4memas0n.Chats.utils;

import org.jetbrains.annotations.NotNull;

public enum Placeholder {
    BROADCAST_PREFIX("{broadcast-prefix}"),
    CHANNEL("{name}"),
    CHANNEL_NICK("{nick}"),
    CHANNEL_COLOR("{color}"),
    CONVERSION_ADDRESS("{con-address}"),
    CONVERSION_PARTNER("{con-partner}"),
    MESSAGE("{message}"),
    SENDER("{sender}"),
    SENDER_PLAIN("{sender-plain}"),
    SENDER_WORLD("{world}"),
    SENDER_PREFIX("{sender-prefix}"),
    SENDER_GROUP("{group}"),
    SENDER_GROUP_PREFIX("{group-prefix"),
    SENDER_GROUP_SUFFIX("{group-suffix"),
    SENDER_SUFFIX("{sender-suffix}");

    private final String placeholder;

    Placeholder(@NotNull final String placeholder) {
        this.placeholder = placeholder.toLowerCase();
    }

    @Override
    @NotNull
    public String toString() {
        return this.placeholder;
    }
}