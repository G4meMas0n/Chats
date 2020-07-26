package de.g4memas0n.chats.storage.cache;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;
import java.util.Map;

/**
 * Representation of a Cache with a specified key and value type.
 *
 * @author G4meMas0n
 * @param <K> the type of the keys of this cache.
 * @param <V> the type of the values of this cache.
 * @since Release 1.0.0
 */
public interface ICache<K, V> {

    /**
     * Returns the value of the entry with the given key.
     *
     * <p>Can be null when there is no entry with the given key.</p>
     *
     * @param key the key of the values entry.
     * @return the value of the founded entry.
     */
    @Nullable V get(@NotNull final K key);

    /**
     * Returns all entries with the given keys.
     *
     * <p>Can be empty when there are no entries with the given keys.</p>
     *
     * @param keys the keys of the entries.
     * @return the founded entries.
     */
    @SuppressWarnings("unused")
    @NotNull Map<K, V> getAll(@NotNull final Collection<K> keys);

    /**
     * Returns all entries in this cache.
     *
     * <p>Can be empty when this cache is empty.</p>
     *
     * @return the entries of this cache.
     */
    @NotNull Map<K, V> getAll();

    /**
     * Removes the entry with the given key from this cache.
     *
     * @param key the key of the entry to remove.
     */
    void invalidate(@NotNull final K key);

    /**
     * Removes all entries with the given keys from this cache.
     *
     * @param keys the keys of the entries to remove.
     */
    @SuppressWarnings("unused")
    void invalidateAll(@NotNull final Collection<K> keys);

    /**
     * Removes all entries from this cache.
     */
    @SuppressWarnings("unused")
    void invalidateAll();

    /**
     * Adds a new entry with the given key and the given value to this cache.
     *
     * @param key the key of the entry to add.
     * @param value the value of the entry to add.
     */
    @SuppressWarnings("unused")
    void put(@NotNull final K key, @NotNull final V value);

    /**
     * Adds all given entries to this cache.
     *
     * @param entries the entries to add.
     */
    @SuppressWarnings("unused")
    void putAll(@NotNull final Map<K, V> entries);

    /**
     * Returns the current count of entries in this cache.
     *
     * @return the size of this cache.
     */
    int size();
}
