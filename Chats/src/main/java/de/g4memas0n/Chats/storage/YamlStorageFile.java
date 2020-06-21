package de.g4memas0n.chats.storage;

import org.bukkit.ChatColor;
import org.bukkit.Color;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.bukkit.util.Vector;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Representation of the storage file, implements {@link IStorageFile}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 29th, 2020
 * changed: June 7th, 2020
 */
public class YamlStorageFile extends YamlConfiguration implements IStorageFile {

    private final File file;
    private String name;

    public YamlStorageFile(@NotNull final File parent,
                           @NotNull final String name) throws IllegalArgumentException {
        this(new File(parent, name + ".yml"));

        this.name = name;
    }

    public YamlStorageFile(@NotNull final File file) throws IllegalArgumentException {
        if (file.exists() && !file.isFile()) {
            throw new IllegalArgumentException(String.format("File can not be a directory: %s", file));
        }

        if (!file.getName().endsWith(".yml") && !file.getName().endsWith(".YML")) {
            throw new IllegalArgumentException(String.format("File is missing YAML extension: %s", file));
        }

        this.file = file;
    }

    @Override
    public @NotNull File getFile() {
        return this.file;
    }

    @Override
    public @NotNull String getName() {
        if (this.name == null) {
            this.name = this.file.getName().substring(0, this.file.getName().lastIndexOf("."));
        }

        return this.name;
    }

    @Override
    public synchronized void clear() {
        try {
            this.loadFromString(BLANK_CONFIG);
        } catch (InvalidConfigurationException ignored) {
            // A blank config can not be invalid.
        }
    }

    @Override
    public synchronized void delete() throws IOException {
        if (!this.file.exists()) {
            throw new IOException(String.format("File does not exist: %s", this.file));
        }

        if (!this.file.delete()) {
            throw new IOException(String.format("File can not be deleted: %s", this.file));
        }
    }

    @Override
    public synchronized void load() throws IOException, InvalidStorageFileException, MissingStorageFileException {
        try {
            super.load(this.file);
        } catch (FileNotFoundException ex) {
            throw new MissingStorageFileException(this.file, ex);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(this.file, ex);
        }
    }

    @Override
    public synchronized void save() throws IOException {
        super.save(file);
    }

    @Override
    public synchronized boolean getBoolean(@NotNull final String path) {
        return super.getBoolean(path);
    }

    @Override
    public synchronized boolean getBoolean(@NotNull final String path, final boolean def) {
        return super.getBoolean(path, def);
    }

    @Override
    public synchronized @NotNull List<Boolean> getBooleanList(@NotNull final String path) {
        return super.getBooleanList(path);
    }

    @Override
    public synchronized @NotNull List<Byte> getByteList(@NotNull final String path) {
        return super.getByteList(path);
    }

    @Override
    public synchronized @NotNull List<Character> getCharacterList(@NotNull final String path) {
        return super.getCharacterList(path);
    }

    @Override
    public synchronized @Nullable ChatColor getChatColor(@NotNull final String path) {
        return this.getChatColor(path, null);
    }

    @Override
    public synchronized @Nullable ChatColor getChatColor(@NotNull final String path, @Nullable final ChatColor def) {
        final String color = super.getString(path);

        if (color == null) {
            return def;
        }

        try {
            return ChatColor.valueOf(color);
        } catch (IllegalArgumentException ignored) {
            return def;
        }
    }

    @Override
    public synchronized @Nullable Color getColor(@NotNull final String path) {
        return super.getColor(path);
    }

    @Override
    public synchronized @Nullable Color getColor(@NotNull final String path, @Nullable final Color def) {
        return super.getColor(path, def);
    }

    @Override
    public synchronized @Nullable ConfigurationSection getConfigurationSection(@NotNull final String path) {
        return super.getConfigurationSection(path);
    }

    @Override
    public synchronized double getDouble(@NotNull final String path) {
        return super.getDouble(path);
    }

    @Override
    public synchronized double getDouble(@NotNull final String path, final double def) {
        return super.getDouble(path, def);
    }

    @Override
    public synchronized @NotNull List<Double> getDoubleList(@NotNull final String path) {
        return super.getDoubleList(path);
    }

    @Override
    public synchronized @NotNull List<Float> getFloatList(@NotNull final String path) {
        return super.getFloatList(path);
    }

    @Override
    public synchronized int getInt(@NotNull final String path) {
        return super.getInt(path);
    }

    @Override
    public synchronized int getInt(@NotNull final String path, final int def) {
        return super.getInt(path, def);
    }

    @Override
    public synchronized @NotNull List<Integer> getIntegerList(@NotNull final String path) {
        return super.getIntegerList(path);
    }

    @Override
    public synchronized @Nullable ItemStack getItemStack(@NotNull final String path) {
        return super.getItemStack(path);
    }

    @Override
    public synchronized @Nullable ItemStack getItemStack(@NotNull final String path, @Nullable final ItemStack def) {
        return super.getItemStack(path, def);
    }

    @Override
    public synchronized @NotNull Set<String> getKeys(final boolean deep) {
        return super.getKeys(deep);
    }

    @Override
    public synchronized @Nullable List<?> getList(@NotNull final String path) {
        return super.getList(path);
    }

    @Override
    public synchronized @Nullable List<?> getList(@NotNull final String path, @Nullable final List<?> def) {
        return super.getList(path, def);
    }

    @Override
    public synchronized @Nullable Locale getLocale(@NotNull final String path) {
        return this.getLocale(path, null);
    }

    @Override
    public synchronized @Nullable Locale getLocale(@NotNull final String path, @Nullable final Locale def) {
        final String locale = super.getString(path);

        if (locale != null && !locale.isEmpty()) {
            final String[] parts = locale.split("_");

            if (parts.length == 1) {
                return new Locale(parts[0]);
            } else if (parts.length == 2) {
                return new Locale(parts[0], parts[1]);
            } else if (parts.length == 3) {
                return new Locale(parts[0], parts[1], parts[2]);
            }
        }

        return def;
    }

    @Override
    public synchronized @Nullable Location getLocation(@NotNull final String path) {
        return super.getLocation(path);
    }

    @Override
    public synchronized @Nullable Location getLocation(@NotNull final String path, @Nullable final Location def) {
        return super.getLocation(path, def);
    }

    @Override
    public synchronized long getLong(@NotNull final String path) {
        return super.getLong(path);
    }

    @Override
    public synchronized long getLong(@NotNull final String path,
                                     final long def) {
        return super.getLong(path, def);
    }

    @Override
    public synchronized @NotNull List<Long> getLongList(@NotNull final String path) {
        return super.getLongList(path);
    }

    @Override
    public synchronized @NotNull List<Map<?, ?>> getMapList(@NotNull final String path) {
        return super.getMapList(path);
    }

    @Override
    public synchronized @Nullable Object get(@NotNull final String path) {
        return super.get(path);
    }

    @Override
    public synchronized @Nullable Object get(@NotNull final String path,
                                             @Nullable final Object def) {
        return super.get(path, def);
    }

    @Override
    public synchronized @Nullable OfflinePlayer getOfflinePlayer(@NotNull final String path) {
        return super.getOfflinePlayer(path);
    }

    @Override
    public synchronized @Nullable OfflinePlayer getOfflinePlayer(@NotNull final String path,
                                                                 @Nullable final OfflinePlayer def) {
        return super.getOfflinePlayer(path, def);
    }

    @Override
    public synchronized @NotNull List<Short> getShortList(@NotNull final String path) {
        return super.getShortList(path);
    }

    @Override
    public synchronized @Nullable String getString(@NotNull final String path) {
        return super.getString(path);
    }

    @Override
    public synchronized @Nullable String getString(@NotNull final String path,
                                                   @Nullable final String def) {
        return super.getString(path, def);
    }

    @Override
    public synchronized @NotNull List<String> getStringList(@NotNull final String path) {
        return super.getStringList(path);
    }

    @Override
    public synchronized @Nullable UUID getUniqueId(@NotNull final String path) {
        return this.getUniqueId(path, null);
    }

    @Override
    public synchronized @Nullable UUID getUniqueId(@NotNull final String path, @Nullable final UUID def) {
        try {
            final String uuid = super.getString(path);

            if (uuid == null) {
                return def;
            }

            return UUID.fromString(uuid);
        } catch (IllegalArgumentException ignored) {
            return def;
        }
    }

    @Override
    public synchronized @NotNull List<UUID> getUniqueIdList(@NotNull final String path) {
        final List<UUID> uuidList = new ArrayList<>();

        for (final String current : super.getStringList(path)) {
            try {
                uuidList.add(UUID.fromString(current));
            } catch (IllegalArgumentException ignored) {

            }
        }

        return uuidList;
    }

    @Override
    public synchronized @NotNull Map<String, Object> getValues(final boolean deep) {
        return super.getValues(deep);
    }

    @Override
    public synchronized @Nullable Vector getVector(@NotNull final String path) {
        return super.getVector(path);
    }

    @Override
    public synchronized @Nullable Vector getVector(@NotNull final String path, @Nullable final Vector def) {
        return super.getVector(path, def);
    }

    @Override
    public synchronized boolean isBoolean(@NotNull final String path) {
        return super.isBoolean(path);
    }

    @Override
    public synchronized boolean isChatColor(@NotNull final String path) {
        if (!this.contains(path) || !this.isString(path)) {
            return false;
        }

        return this.getChatColor(path) != null;
    }

    @Override
    public synchronized boolean isColor(@NotNull final String path) {
        return super.isColor(path);
    }

    @Override
    public synchronized boolean isConfigurationSection(@NotNull final String path) {
        return super.isConfigurationSection(path);
    }

    @Override
    public synchronized boolean isDouble(@NotNull final String path) {
        return super.isDouble(path);
    }

    @Override
    public synchronized boolean isInt(@NotNull final String path) {
        return super.isInt(path);
    }

    @Override
    public synchronized boolean isItemStack(@NotNull final String path) {
        return super.isItemStack(path);
    }

    @Override
    public synchronized boolean isList(@NotNull final String path) {
        return super.isList(path);
    }

    @Override
    public synchronized boolean isLocale(@NotNull final String path) {
        if (!this.contains(path) || !this.isString(path)) {
            return false;
        }

        return this.getLocale(path) != null;
    }

    @Override
    public synchronized boolean isLocation(@NotNull final String path) {
        return super.isLocation(path);
    }

    @Override
    public synchronized boolean isLong(@NotNull final String path) {
        return super.isLong(path);
    }

    @Override
    public synchronized boolean isOfflinePlayer(@NotNull final String path) {
        return super.isOfflinePlayer(path);
    }

    @Override
    public synchronized boolean isSet(@NotNull final String path) {
        return super.isSet(path);
    }

    @Override
    public synchronized boolean isString(@NotNull final String path) {
        return super.isString(path);
    }

    @Override
    public synchronized boolean isUniqueId(@NotNull final String path) {
        if (!this.contains(path) || !this.isString(path)) {
            return false;
        }

        return this.getUniqueId(path) != null;
    }

    @Override
    public synchronized boolean isVector(@NotNull final String path) {
        return super.isVector(path);
    }

    @Override
    public synchronized void set(@NotNull final String path, @Nullable final Object value) {
        super.set(path, value);
    }
}
