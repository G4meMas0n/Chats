package de.g4memas0n.chats.storage.cache;

import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.util.logging.BasicLogger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Unique-id Cache, that caches player names with their unique-id.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public class UniqueIdCache implements ICache<String, UUID>, IStorageHolder {

    private final YamlStorageFile storage;
    private final BasicLogger logger;

    public UniqueIdCache(@NotNull final YamlStorageFile storage, @NotNull final BasicLogger logger) {
        this.storage = storage;
        this.logger = logger;
    }

    @Override
    public synchronized void delete() {
        try {
            this.storage.delete();

            this.logger.debug(String.format("Deleted storage file '%s' of unique-id cache.",
                    this.storage.getFile().getName()));
        } catch (IOException ex) {
            this.logger.debug(String.format("Unable to delete storage file '%s' of unique-id cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            this.logger.debug(String.format("Loaded storage file '%s' of unique-id cache.",
                    this.storage.getFile().getName()));
        } catch (MissingStorageFileException ex) {
            this.logger.debug(String.format("Unable to find storage file '%s' of unique-id cache.",
                    this.storage.getFile().getName()));

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            this.logger.debug(String.format("Unable to load storage file '%s' of unique-id cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void save() {
        try {
            this.storage.save();

            this.logger.debug(String.format("Saved storage file '%s' of unique-id cache.",
                    this.storage.getFile().getName()));
        } catch (IOException ex) {
            this.logger.debug(String.format("Unable to save storage file '%s' of unique-id cache: %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    public boolean exists() {
        return this.storage.getFile().exists();
    }

    @Override
    public @Nullable UUID get(@NotNull final String key) {
        return this.storage.getUniqueId(key.toLowerCase());
    }

    @Override
    public @NotNull Map<String, UUID> getAll(@NotNull final Collection<String> keys) {
        final Map<String, UUID> values = new LinkedHashMap<>();

        for (final String key : keys) {
            final UUID uniqueId = this.storage.getUniqueId(key.toLowerCase());

            if (uniqueId != null) {
                values.put(key, uniqueId);
            }
        }

        return values;
    }

    @Override
    public @NotNull Map<String, UUID> getAll() {
        final Map<String, UUID> values = new LinkedHashMap<>();

        for (final String key : this.storage.getKeys(false)) {
            final UUID uniqueId = this.storage.getUniqueId(key);

            if (uniqueId != null) {
                values.put(key, uniqueId);
            }
        }

        return values;
    }

    @Override
    public void invalidate(@NotNull final String key) {
        this.storage.set(key.toLowerCase(), null);
    }

    @Override
    public void invalidateAll(@NotNull final Collection<String> keys) {
        for (final String key : keys) {
            this.storage.set(key.toLowerCase(), null);
        }
    }

    @Override
    public void invalidateAll() {
        this.storage.clear();
    }

    @Override
    public void put(@NotNull final String key, @NotNull final UUID value) {
        this.storage.set(key.toLowerCase(), value);
    }

    @Override
    public void putAll(@NotNull final Map<String, UUID> values) {
        for (final Map.Entry<String, UUID> entry : values.entrySet()) {
            this.storage.set(entry.getKey().toLowerCase(), entry.getValue());
        }
    }

    @Override
    public int size() {
        return this.storage.getKeys(false).size();
    }
}
