package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ModifyType Enum for all types a channel can be modified.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 3rd, 2020
 * changed: June 20th, 2020
 */
public enum ModifyType implements Type {

    /**
     * Represents the announce format modify option of a channel.
     */
    ANNOUNCE_FORMAT("announce-format", "announceFormat", true, false, true),

    /**
     * Represents the broadcast-format modify option of a channel.
     */
    BROADCAST_FORMAT("broadcast-format", "broadcastFormat", true, false, true),

    /**
     * Represents the chat-format modify option of a channel.
     */
    CHAT_FORMAT("chat-format", "chatFormat", true, false, true),

    /**
     * Represents the color modify option of a channel.
     */
    COLOR("color", false, true, true),

    /**
     * Represents the cross-world modify option of a channel.
     */
    CROSS_WORLD("cross-world", "crossWorld", false, true, true),

    /**
     * Represents the custom-format modify option of a channel.
     */
    CUSTOM_FORMAT("custom-format", "customFormat", false, true, true),

    /**
     * Represents the distance modify option of a channel.
     */
    DISTANCE("distance", true, false, true),

    /**
     * Represents the moderators modify options of a channel.
     */
    MODERATORS("moderators", false, false, false),

    /**
     * Represents the owner modify option of a channel.
     */
    OWNER("owner", true, false, true),

    /**
     * Represents the password modify option of a channel.
     */
    PASSWORD("password", true, false, true),

    /**
     * Represents the short-name modify option of a channel.
     */
    SHORT_NAME("short-name", "shortName", true, false, true);

    private final String identifier;
    private final String key;

    private final boolean removable;
    private final boolean resettable;
    private final boolean settable;

    ModifyType(@NotNull final String identifier,
               final boolean removable,
               final boolean resettable,
               final boolean settable) {
        this.identifier = identifier;
        this.key = identifier;

        this.removable = removable;
        this.resettable = resettable;
        this.settable = settable;
    }

    ModifyType(@NotNull final String identifier,
               @NotNull final String key,
               final boolean removable,
               final boolean resettable,
               final boolean settable) {
        this.identifier = identifier;
        this.key = key;

        this.removable = removable;
        this.resettable = resettable;
        this.settable = settable;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final @NotNull String getKey() {
        return this.key;
    }

    public final boolean isRemovable() {
        return this.removable;
    }

    public final boolean isResettable() {
        return this.resettable;
    }

    public final boolean isSettable() {
        return this.settable;
    }

    @Override
    public final @NotNull String toString() {
        return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.key + "}";
    }

    /**
     * Returns the modify type with the given identifier. Can be null when there is no modify type with the given
     * identifier;
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
