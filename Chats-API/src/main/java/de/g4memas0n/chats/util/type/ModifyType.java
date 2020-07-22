package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ModifyType Enum for all types a channel can be modified.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public enum ModifyType implements Type {

    /**
     * Represents the announce format modify option of a channel.
     */
    ANNOUNCE_FORMAT("announce-format"),

    /**
     * Represents the broadcast-format modify option of a channel.
     */
    BROADCAST_FORMAT("broadcast-format"),

    /**
     * Represents the chat-format modify option of a channel.
     */
    CHAT_FORMAT("chat-format"),

    /**
     * Represents the color modify option of a channel.
     */
    COLOR("color"),

    /**
     * Represents the cross-world modify option of a channel.
     */
    CROSS_WORLD("cross-world"),

    /**
     * Represents the custom-format modify option of a channel.
     */
    CUSTOM_FORMAT("custom-format"),

    /**
     * Represents the distance modify option of a channel.
     */
    DISTANCE("distance"),

    /**
     * Represents the owner modify option of a channel.
     */
    OWNER("owner"),

    /**
     * Represents the password modify option of a channel.
     */
    PASSWORD("password"),

    /**
     * Represents the short-name modify option of a channel.
     */
    SHORT_NAME("short-name"),

    /**
     * Represents the verbose modify option of a channel.
     */
    VERBOSE("verbose");

    private final String identifier;

    ModifyType(@NotNull final String identifier) {
        this.identifier = identifier;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final @NotNull String getKey() {
        final int index = this.identifier.indexOf("-");

        if (index < 0) {
            return this.identifier;
        }

        if (identifier.length() == index + 1) {
            return this.identifier.substring(0, index);
        }

        return this.identifier.substring(0, index) + this.identifier.substring(index + 1, index + 2).toUpperCase()
                + this.identifier.substring(index + 2);
    }

    @Override
    public final @NotNull String toString() {
        return this.getClass().getSimpleName() + "{identifier=" + this.getIdentifier() + ";key=" + this.getKey() + "}";
    }

    /**
     * Returns the modify type with the given identifier.
     *
     * <p>Can be null when there is no modify type with the given identifier.</p>
     *
     * @param identifier the identifier to search for the type.
     * @return the modify type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable ModifyType getType(@NotNull final String identifier) {
        for (final ModifyType current : values()) {
            if (current.getIdentifier().equalsIgnoreCase(identifier)) {
                return current;
            }
        }

        return null;
    }
}
