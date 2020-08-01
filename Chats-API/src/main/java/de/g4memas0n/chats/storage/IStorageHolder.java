package de.g4memas0n.chats.storage;

import de.g4memas0n.chats.util.IType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * StorageHolder Interface that provide storage methods for Objects with a persistent storage.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public interface IStorageHolder {

    /**
     * Deletes the storage file for this storage holder.
     *
     * <p>Any errors deleting the storage file will be logged and then ignored.</p>
     */
    void delete();

    /**
     * (Re)Loads the storage file and updates this storage holder to the new state.
     *
     * <p>Any errors loading the storage file will be logged and the previous state will be kept.</p>
     */
    void load();

    /**
     * Saves the storage file of this storage holder.
     *
     * <p>Any errors saving the storage file will be logged and then ignored.</p>
     */
    void save();

    /**
     * Type Enum for all types of storage holders that can be reloaded or saved.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    enum Type implements IType {

        /**
         * Represents all storage-holders that can be reloaded or saved.
         */
        ALL("all"),

        /**
         * Represents the channel storage-holder.
         */
        CHANNEL("channel", "channels"),

        /**
         * Represents the chatter storage-holder.
         */
        CHATTER("chatter", "chatters"),

        /**
         * Represents the config storage-holder.
         */
        CONFIG("config");

        private final String identifier;
        private final String key;

        Type(@NotNull final String identifier) {
            this.identifier = identifier;
            this.key = null;
        }

        Type(@NotNull final String identifier, @NotNull final String key) {
            this.identifier = identifier;
            this.key = key;
        }

        @Override
        public @NotNull String getIdentifier() {
            return this.identifier;
        }

        @Override
        public @NotNull String getKey() {
            return this.key != null ? this.key : this.identifier;
        }

        @Override
        public final @NotNull String toString() {
            return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.getKey() + "}";
        }

        /**
         * Returns the default storage type.
         *
         * @return the default storage type.
         */
        public static @NotNull Type getDefault() {
            return Type.CONFIG;
        }

        /**
         * Returns the reload type with the given identifier.
         *
         * <p>Can be null when there is no reload type with the given identifier.</p>
         *
         * @param identifier the identifier to search for the type.
         * @return the reload type with the given identifier or null if there is no with the given identifier.
         */
        public static @Nullable Type getType(@NotNull final String identifier) {
            for (final Type type : Type.values()) {
                if (type.getIdentifier().equalsIgnoreCase(identifier)) {
                    return type;
                }
            }

            return null;
        }
    }
}
