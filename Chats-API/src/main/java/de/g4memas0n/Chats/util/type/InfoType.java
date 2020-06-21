package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * InfoType Enum for all types of channel information's.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: March 17th, 2020
 * changed: June 7th, 2020
 */
public enum InfoType implements Type {

    /*
     * Represents the bans info of a channel
     */
    //BANS("bans"),

    /**
     * Represents the color info of a channel.
     */
    COLOR("color"),

    /**
     * Represents the cross-world info of a channel.
     */
    CROSS_WORLD("cross-world", "crossWorld"),

    /**
     * Represents the distance info of a channel.
     */
    DISTANCE("distance"),

    /**
     * Represents the formats info of a channel.
     */
    FORMATS("formats"),

    /**
     * Represents the moderators info of a channel.
     */
    MODERATORS("moderators"),

    /**
     * Represents the mutes info of a channel.
     */
    MUTES("mutes"),

    /**
     * Represents the owner info of a channel.
     */
    OWNER("owner"),

    /**
     * Represents the password info of a channel.
     */
    PASSWORD("password"),

    /**
     * Represents the short-name info of a channel.
     */
    SHORT_NAME("short-name", "shortName"),

    /**
     * Represents the channel type info of a channel.
     */
    TYPE("type");

    private final String identifier;
    private final String key;

    InfoType(@NotNull final String identifier) {
        this.identifier = identifier;
        this.key = identifier;
    }

    InfoType(@NotNull final String identifier, @NotNull final String key) {
        this.identifier = identifier;
        this.key = key;
    }

    @Override
    public final @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public final @NotNull String getKey() {
        return this.key;
    }

    @Override
    public final @NotNull String toString() {
        return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.key + "}";
    }

    /**
     * Returns the info type with the given identifier. Can be null when there is no info type with the given
     * identifier;
     * @param identifier the identifier to search for the type.
     * @return the info type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable InfoType getType(@NotNull final String identifier) {
        for (final InfoType current : values()) {
            if (current.getIdentifier().equalsIgnoreCase(identifier)) {
                return current;
            }
        }

        return null;
    }
}
