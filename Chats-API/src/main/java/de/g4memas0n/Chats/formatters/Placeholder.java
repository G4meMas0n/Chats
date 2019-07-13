package de.g4memas0n.Chats.formatters;

import org.jetbrains.annotations.NotNull;

public enum Placeholder {
    BROADCAST_PREFIX("{broadcast-prefix}"),
    CHANNEL("{name}"),
    CHANNEL_COLOR("{color}"),
    CHANNEL_NICK("{nick}"),
    CON_ADDRESS("{con-address}"),
    CON_PARTNER("{con-partner}"),
    //CON_PARTNER_FORMAT("{con-partner-format}"),
    CON_PARTNER_GROUP("{con-partner-group}"),
    CON_PARTNER_GROUP_PREFIX("{con-partner-group-prefix}"),
    CON_PARTNER_GROUP_SUFFIX("{con-partner-group-suffix}"),
    CON_PARTNER_PLAIN("{con-partner-plain}"),
    CON_PARTNER_PREFIX("{con-partner-prefix}"),
    CON_PARTNER_SUFFIX("{con-partner-suffix}"),
    CON_PARTNER_WORLD("{con-partner-world}"),
    MESSAGE("{message}"),
    SENDER("{sender}"),
    //SENDER_FORMAT("{sender-format}"),
    SENDER_GROUP("{group}"),
    SENDER_GROUP_PREFIX("{group-prefix}"),
    SENDER_GROUP_SUFFIX("{group-suffix}"),
    SENDER_PLAIN("{sender-plain}"),
    SENDER_PREFIX("{sender-prefix}"),
    SENDER_SUFFIX("{sender-suffix}"),
    SENDER_WORLD("{sender-world}");

    private final String placeholder;

    Placeholder(@NotNull final String placeholder) {
        this.placeholder = placeholder.toLowerCase();
    }

    @NotNull
    public static String stripPlaceholders(@NotNull String input) {
        for (Placeholder current : Placeholder.values()) {
            if (input.contains(current.toString())) {
                input = input.replaceAll(current.toString(), "");
            }
        }

        return input;
    }

    @Override
    @NotNull
    public String toString() {
        return this.placeholder;
    }
}