package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ChannelType Enum for all types of channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public enum ChannelType implements Type {

    /**
     * Represents a conversation channel.
     */
    CONVERSATION("conversation"),

    /**
     * Represents a persistent channel.
     */
    PERSIST("persist"),

    /**
     * Represents a standard not persistent channel.
     */
    STANDARD("standard");

    private final String identifier;

    ChannelType(@NotNull final String identifier) {
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
     * Returns the default channel type.
     *
     * @return the default channel type.
     */
    public static @NotNull ChannelType getDefault() {
        return ChannelType.STANDARD;
    }

    /**
     * Returns the channel type with the given identifier.
     *
     * <p>Can be null when there is no channel type with the given identifier.</p>
     *
     * @param identifier the identifier to search for the type.
     * @return the channel type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable ChannelType getType(@NotNull final String identifier) {
        for (final ChannelType current : ChannelType.values()) {
            if (current.getIdentifier().equalsIgnoreCase(identifier)) {
                return current;
            }
        }

        return null;
    }
}
