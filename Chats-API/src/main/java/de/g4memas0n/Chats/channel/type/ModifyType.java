package de.g4memas0n.Chats.channel.type;

import de.g4memas0n.Chats.util.Type;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ModifyType Enum for all types a channel can be modified.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 3rd, 2020
 * changed: February 3rd, 2020
 */
public enum ModifyType implements Type {
    ANNOUNCE_FORMAT("announce-format"),
    BROADCAST_FORMAT("broadcast-format"),
    CHAT_FORMAT("chat-format"),
    CROSS_WORLD("cross-world"),
    COLOR("color"),
    DISTANCE("distance"),
    PASSWORD("password"),
    SHORT_NAME("short-name"),
    USE_CUSTOM_FORMAT("use-custom-format");

    private final String identifier;

    ModifyType(@NotNull final String identifier) {
        this.identifier = identifier;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final String toString() {
        return "ModifyType{identifier='" + this.getIdentifier() + "'}";
    }

    /**
     * Returns the modify type with the given identifier. Can be null when there is no modify type with the given
     * identifier;
     * @param identifier the identifier to search for the type.
     * @return the modify type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable ModifyType getType(@NotNull final String identifier) {
        for (final ModifyType current : values()) {
            if (current.getIdentifier().equals(identifier)) {
                return current;
            }
        }

        return null;
    }
}
