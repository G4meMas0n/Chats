package de.g4memas0n.Chats.storage;

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
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 29th, 2020
 * changed: January 29th, 2020
 */
public interface IStorageFile extends Configuration {

    void load() throws IOException, InvalidStorageFileException;

    void delete();

    void save() throws IOException;

    @NotNull File getFile();

    @Nullable ChatColor getChatColor(@NotNull final String path);

    @Nullable ChatColor getChatColor(@NotNull final String path, @Nullable final ChatColor def);

    @Nullable Locale getLocale(@NotNull final String path);

    @Nullable Locale getLocale(@NotNull final String path, @Nullable final Locale def);

    @Nullable UUID getUniqueId(@NotNull final String path);

    @Nullable UUID getUniqueId(@NotNull final String path, @Nullable final UUID def);

    @NotNull List<UUID> getUniqueIdList(@NotNull final String path);

    boolean isChatColor(@NotNull final String path);

    boolean isLocale(@NotNull final String path);

    boolean isUniqueId(@NotNull final String path);
}
