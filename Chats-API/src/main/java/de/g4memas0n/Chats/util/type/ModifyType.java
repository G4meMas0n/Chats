package de.g4memas0n.Chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ModifyType Enum for all types a channel can be modified.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 3rd, 2020
 * changed: March 3rd, 2020
 */
public enum ModifyType implements Type {
    ANNOUNCE_FORMAT("announce", true, true),
    BROADCAST_FORMAT("broadcast", true, true),
    CHAT_FORMAT("chat", true, false),
    COLOR("color", true, false),
    CROSS_WORLD("cross-world", true, false),
    CUSTOM_FORMAT("custom-format", true, false),
    DISTANCE("distance", true, false),
    PASSWORD("password", true, true),
    SHORT_NAME("short-name", true, true);

    private final String identifier;

    private final boolean adjustable;
    private final boolean removable;

    ModifyType(@NotNull final String identifier,
               final boolean adjustable,
               final boolean removable) {
        this.identifier = identifier;

        this.adjustable = adjustable;
        this.removable = removable;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    public final boolean isAdjustable() {
        return this.adjustable;
    }

    public final boolean isRemovable() {
        return this.removable;
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
