package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ReloadType Enum for all types a permissible can reload.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 3rd, 2020
 * changed: May 31th, 2020
 */
public enum StorageType implements Type {

    /**
     * Represents the option for reloading everything.
     */
    ALL("all"),

    /**
     * Represents the option for reloading the channels.
     */
    CHANNEL("channel"),

    /**
     * Represents the option for reloading the chatters.
     */
    CHATTER("chatter"),

    /**
     * Represents the option for reloading the configuration.
     */
    CONFIG("config");

    private final String identifier;

    StorageType(@NotNull final String identifier) {
        this.identifier = identifier;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final @NotNull String getKey() {
        return this.identifier;
    }

    @Override
    public final @NotNull String toString() {
        return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.identifier + "}";
    }

    /**
     * Returns the default reload type.
     * @return the default reload type.
     */
    public static @NotNull StorageType getDefault() {
        return StorageType.CONFIG;
    }

    /**
     * Returns the reload type with the given identifier. Can be null when there is no reload type with the given
     * identifier;
     * @param identifier the identifier to search for the type.
     * @return the reload type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable StorageType getType(@NotNull final String identifier) {
        for (final StorageType current : StorageType.values()) {
            if (current.getIdentifier().equalsIgnoreCase(identifier)) {
                return current;
            }
        }

        return null;
    }
}
