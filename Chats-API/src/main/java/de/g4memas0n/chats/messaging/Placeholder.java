package de.g4memas0n.chats.messaging;

import org.jetbrains.annotations.NotNull;

/**
 * Placeholder Enum for all Placeholders that are used for all formats specified in the plugin configuration file.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public enum Placeholder {

    /**
     * Represents the color placeholder that will be replaced with the channels color code.
     */
    CHANNEL_COLOR("{color}"),

    /**
     * Represents the name placeholder that will be replaced with the channels full name.
     */
    CHANNEL_NAME("{name}"),

    /**
     * Represents the nick placeholder that will be replaced with the channels short name.
     */
    CHANNEL_NICK("{nick}"),

    /**
     * Represents the conversation address placeholder that will be replaced with "to" or "from".
     */
    CON_ADDRESS("{con-address}"),

    /**
     * Represents the conversation partner placeholder that will be replaced with the partners display name.
     */
    CON_PARTNER("{con-partner}"),

    /**
     * Represents the message placeholder that will be replaced with message.
     */
    MESSAGE("{message}"),

    /**
     * Represents the sender placeholder that will be replaced with the senders display name.
     */
    SENDER("{sender}"),

    /**
     * Represents the sender plain placeholder that will be replaced with the senders name.
     */
    SENDER_PLAIN("{sender-plain}"),

    /**
     * Represents the group placeholder that will be replaced with the senders group name.
     *
     * <p><i><b>Note:</b> This only works with Vault.</i></p>
     */
    SENDER_GROUP("{group}"),

    /**
     * Represents the group prefix placeholder that will be replaced with the senders group prefix.
     *
     * <p><i><b>Note:</b> This only works with Vault.</i></p>
     */
    SENDER_GROUP_PREFIX("{group-prefix}"),

    /**
     * Represents the group suffix placeholder that will be replaced with the senders group suffix.
     *
     * <p><i><b>Note:</b> This only works with Vault.</i></p>
     */
    SENDER_GROUP_SUFFIX("{group-suffix}"),

    /**
     * Represents the sender prefix placeholder that will be replaced with the senders prefix.
     *
     * <p><i><b>Note:</b> This only works with Vault.</i></p>
     */
    SENDER_PREFIX("{sender-prefix}"),

    /**
     * Represents the sender suffix placeholder that will be replaced with the senders suffix.
     *
     * <p><i><b>Note:</b> This only works with Vault.</i></p>
     */
    SENDER_SUFFIX("{sender-suffix}"),

    /**
     * Represents the world placeholder that will be replaced with the senders world name.
     */
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
     *
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
