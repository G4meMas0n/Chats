package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;

/**
 * Placeholder Enum for all Placeholders that are used for all formats specified in the plugin configuration file.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * changed: September 11th, 2019
 */
public enum Placeholder {
    BROADCAST_PREFIX("{bc-prefix}"),
    CHANNEL_COLOR("{color}"),
    CHANNEL_NAME("{name}"),
    CHANNEL_NICK("{nick}"),
    CON_ADDRESS("{con-address}"),
    CON_PARTNER("{con-partner}"),
    CON_SENDER("{con-sender}"),
    MESSAGE("{message}"),
    SENDER("{sender}"),
    SENDER_PLAIN("{sender-plain}"),
    SENDER_GROUP("{group}"),
    SENDER_GROUP_PREFIX("{group-prefix}"),
    SENDER_GROUP_SUFFIX("{group-suffix}"),
    SENDER_PREFIX("{sender-prefix}"),
    SENDER_SUFFIX("{sender-suffix}"),
    SENDER_WORLD("{world}");

    private final String placeholder;

    Placeholder(@NotNull final String placeholder) {
        this.placeholder = placeholder.toLowerCase();
    }

    @Override
    public @NotNull String toString() {
        return this.placeholder;
    }

    /**
     * Strips all founded Placeholder contained in the given input string.
     * @param input the string that should be stripped.
     * @return the stripped input string.
     */
    public static @NotNull String stripPlaceholders(@NotNull String input) {
        for (Placeholder current : Placeholder.values()) {
            if (input.contains(current.toString())) {
                input = input.replaceAll(current.toString(), "");
            }
        }

        return input;
    }
}
