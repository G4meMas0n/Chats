package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Set;
import java.util.UUID;

/**
 * Chatter Cache, that caches player names with their uniqueId.
 *
 * @author G4meMason
 * @since 0.2.3-SNAPSHOT
 *
 * created: June 14th, 2020
 * changed: June 19th, 2020
 */
public final class ChatterCache implements IStorageHolder {

    private final IStorageFile storage;

    protected ChatterCache(@NotNull final IStorageFile storage) {
        this.storage = storage;
    }

    @Override
    public @NotNull IStorageFile getStorage() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();

            Log.getPlugin().debug(String.format("Deleted storage file '%s' of username-cache.",
                    this.storage.getFile().getName()));
        } catch (IOException ex) {
            Log.getPlugin().debug(String.format("Unable to delete storage file '%s' of username-cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public void load() {
        try {
            this.storage.load();

            Log.getPlugin().debug(String.format("Loaded username-cache from storage file: %s",
                    this.storage.getFile().getName()));
        } catch (MissingStorageFileException ex) {
            Log.getPlugin().debug(String.format("Unable to find storage file '%s' of username-cache.",
                    this.storage.getFile().getName()));

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPlugin().debug(String.format("Unable to load storage file '%s' of username-cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public void save() {
        try {
            this.storage.save();

            Log.getPlugin().debug(String.format("Saved username-cache to storage file: %s",
                    this.storage.getFile().getName()));
        } catch (IOException ex) {
            Log.getPlugin().debug(String.format("Unable to save storage file '%s' of username-cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    public @NotNull Set<String> getKeys() {
        return this.storage.getKeys(false);
    }

    public @Nullable final UUID get(@NotNull final String name) {
        return this.storage.getUniqueId(name.toLowerCase());
    }

    public void invalidate(@NotNull final String name) {
        this.storage.set(name.toLowerCase(), null);
    }

    public void invalidate(@NotNull final UUID uniqueId) {
        for (final String key : this.storage.getKeys(false)) {
            final UUID value = this.storage.getUniqueId(key);

            if (value == null || value.equals(uniqueId)) {
                this.storage.set(key, null);
            }
        }
    }

    public void put(@NotNull final String name, @NotNull final UUID uniqueId) {
        this.storage.set(name.toLowerCase(), uniqueId.toString());
    }
}
