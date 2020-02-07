package de.g4memas0n.Chats.channel.type;

import de.g4memas0n.Chats.util.Type;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * ChannelType Enum for all types of channels.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 3rd, 2020
 * changed: February 3rd, 2020
 */
public enum ChannelType implements Type {
    CONVERSATION("conversation"),
    PERSIST("persist"),
    STANDARD("standard");

    private final String identifier;

    ChannelType(@NotNull final String identifier) {
        this.identifier = identifier;
    }

    @Override
    public @NotNull String getIdentifier() {
        return this.identifier;
    }

    @Override
    public @NotNull String toString() {
        return "CreateType{identifier='" + this.getIdentifier() + "'}";
    }

    /**
     * Returns the channel type with the given identifier. Can be null when there is no channel type with the given
     * identifier;
     * @param identifier the identifier to search for the type.
     * @return the channel type with the given identifier or null if there is no with the given identifier.
     */
    public static @Nullable ChannelType getType(@NotNull final String identifier) {
        for (final ChannelType current : ChannelType.values()) {
            if (current.getIdentifier().equals(identifier)) {
                return current;
            }
        }

        return null;
    }

    /**
     * Returns the default channel type.
     * @return the default channel type.
     */
    public static @NotNull ChannelType getDefault() {
        return ChannelType.STANDARD;
    }
}
