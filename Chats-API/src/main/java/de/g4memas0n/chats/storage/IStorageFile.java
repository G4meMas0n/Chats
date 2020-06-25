package de.g4memas0n.chats.storage;

import org.bukkit.ChatColor;
import org.bukkit.configuration.Configuration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

/**
 * StorageFile Interface that represents a storage file as a source of configurable options and settings.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 29th, 2020
 * changed: May 20th, 2020
 */
public interface IStorageFile extends Configuration {

    /**
     * Returns the base file of this {@link IStorageFile} that is used for the {@link IStorageFile#load()},
     * {@link IStorageFile#delete()} and {@link IStorageFile#save()} methods.
     * @return the base file where this storage file is located.
     */
    @NotNull File getFile();

    /**
     * Returns the name of the base file of this {@link IStorageFile} without a file extension.
     * @return the base file name of this storage file.
     */
    @NotNull String getName();

    /**
     * Clears this {@link IStorageFile}.
     * All the values contained within this configuration will be removed, leaving only settings and defaults.
     */
    void clear();

    /**
     * Loads this {@link IStorageFile}.
     * All the values contained within this configuration will be removed, leaving only settings and defaults, and
     * the new values will be loaded from the given file.
     * If the base file can not be loaded for any reason, an exception will be thrown.
     * @throws IOException Thrown when the storage file can not be read.
     * @throws InvalidStorageFileException Thrown when the storage file is not a valid configuration.
     * @throws MissingStorageFileException Thrown when the storage file does not exists.
     */
    void load() throws IOException, InvalidStorageFileException, MissingStorageFileException;

    /**
     * Deletes this {@link IStorageFile}.
     * All the values contained within this configuration will be kept, but the base file will be deleted.
     * If the base file can not be deleted for any reason, an exception will be thrown.
     * @throws IOException Thrown when the storage file can not be deleted.
     */
    void delete() throws IOException;

    /**
     * Saves this {@link IStorageFile}.
     * If the base file does not exist, it will be created. If already exists, it will be overwritten.
     * If it can not be overwritten or created, an exception will be thrown.
     * @throws IOException Thrown when the storage file can not be written to for any reason.
     */
    void save() throws IOException;

    /**
     * Gets the requested ChatColor by path.
     * If the ChatColor does not exist, this will return null.
     * @param path The Path of the ChatColor to get.
     * @return The requested ChatColor.
     */
    @Nullable ChatColor getChatColor(@NotNull final String path);

    /**
     * Gets the requested ChatColor by path, returning a default value if not found.
     * If the ChatColor does not exist then the given default value will returned.
     * @param path The Path of the ChatColor to get.
     * @param def The default value to return if the path is not found or is not a ChatColor.
     * @return The requested ChatColor.
     */
    @Nullable ChatColor getChatColor(@NotNull final String path, @Nullable final ChatColor def);

    /**
     * Gets the requested Locale by path.
     * If the Locale does not exist, this will return null.
     * @param path The Path of the Locale to get.
     * @return The requested Locale.
     */
    @Nullable Locale getLocale(@NotNull final String path);

    /**
     * Gets the requested Locale by path, returning a default value if not found.
     * If the Locale does not exist then the given default value will returned.
     * @param path The Path of the Locale to get.
     * @param def The default value to return if the path is not found or is not a Locale.
     * @return The requested Locale.
     */
    @Nullable Locale getLocale(@NotNull final String path, @Nullable final Locale def);

    /**
     * Gets the requested UniqueId by path.
     * If the UniqueId does not exist, this will return null.
     * @param path The Path of the UniqueId to get.
     * @return The requested UniqueId.
     */
    @Nullable UUID getUniqueId(@NotNull final String path);

    /**
     * Gets the requested UniqueId by path, returning a default value if not found.
     * If the UniqueId does not exist then the given default value will returned.
     * @param path The Path of the UniqueId to get.
     * @param def The default value to return if the path is not found or is not a UniqueId.
     * @return The requested UniqueId.
     */
    @Nullable UUID getUniqueId(@NotNull final String path, @Nullable final UUID def);

    /**
     * Gets the requested List of UniqueId by path.
     * If the List does not exist, this will return an empty List.
     * @param path The Path of the List to get.
     * @return The requested List of UniqueId.
     */
    @NotNull List<UUID> getUniqueIdList(@NotNull final String path);

    /**
     * Checks if the specified path is a ChatColor.
     * If the path exists but is not a ChatColor or if the path does not exist, this will return false.
     * @param path The Path of the ChatColor to check.
     * @return Whether or not the specified path is a ChatColor.
     */
    @SuppressWarnings("unused")
    boolean isChatColor(@NotNull final String path);

    /**
     * Checks if the specified path is a Locale.
     * If the path exists but is not a Locale or if the path does not exist, this will return false.
     * @param path The Path of the Locale to check.
     * @return Whether or not the specified path is a Locale.
     */
    @SuppressWarnings("unused")
    boolean isLocale(@NotNull final String path);

    /**
     * Checks if the specified path is a UniqueId.
     * If the path exists but is not a UniqueId or if the path does not exist, this will return false.
     * @param path The Path of the UniqueId to check.
     * @return Whether or not the specified path is a UniqueId.
     */
    @SuppressWarnings("unused")
    boolean isUniqueId(@NotNull final String path);
}
