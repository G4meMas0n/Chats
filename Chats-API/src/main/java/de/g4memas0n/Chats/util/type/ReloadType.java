package de.g4memas0n.Chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ReloadType Enum for all types a permissible can reload.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 3rd, 2020
 * changed: February 10th, 2020
 */
public enum ReloadType implements Type {
    ALL("all"),
    CHANNELS("channels"),
    CHATTERS("chatters"),
    CONFIG("config");

    private final String identifier;

    ReloadType(@NotNull final String identifier) {
        this.identifier = identifier;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final @NotNull String toString() {
        return "ReloadType{identifier='" + this.getIdentifier() + "'}";
    }

    /**
     * Returns the reload type with the given identifier. Can be null when there is no reload type with the given
     * identifier;
     * @param identifier the identifier to search for the type.
     * @return the reload type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable ReloadType getType(@NotNull final String identifier) {
        for (final ReloadType current : ReloadType.values()) {
            if (current.getIdentifier().equalsIgnoreCase(identifier)) {
                return current;
            }
        }

        return null;
    }

    /**
     * Returns the default reload type.
     * @return the default reload type.
     */
    public static @NotNull ReloadType getDefault() {
        return ReloadType.CONFIG;
    }
}
