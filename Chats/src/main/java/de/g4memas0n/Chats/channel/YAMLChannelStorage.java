package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import org.bukkit.ChatColor;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Yaml File Configuration storage of a channel, implements the {@link IChannelStorage} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: November 15th, 2019
 */
public final class YAMLChannelStorage implements IChannelStorage {

    /**
     * the yaml configuration paths of all saved channel options.
     */
    private static final String PATH_FULL_NAME = "name";
    private static final String PATH_SHORT_NAME = "nick";
    private static final String PATH_COLOR = "color";
    private static final String PATH_CROSS_WORLD = "cross-world";
    private static final String PATH_DISTANCE = "distance";
    private static final String PATH_PASSWORD = "password";
    private static final String PATH_FORMAT_ANNOUNCE = "format.announce";
    private static final String PATH_FORMAT_BROADCAST = "format.broadcast";
    private static final String PATH_FORMAT_CHANNEL = "format.channel";
    private static final String PATH_FORMAT_USE_CUSTOM = "format.use-custom";

    private final Map<String, YamlConfiguration> configurations;
    private File directory;

    public YAMLChannelStorage(@NotNull final File directory) throws IllegalArgumentException {
        this.configurations = new HashMap<>();
        this.setDirectory(directory);
    }

    @Override
    public @NotNull File getDirectory() {
        return this.directory;
    }

    @Override
    public boolean setDirectory(@NotNull final File directory) throws IllegalArgumentException {
        if (!directory.isDirectory()) {
            throw new IllegalArgumentException("Invalid File! File must be a directory!");
        }

        if (this.directory.equals(directory)) {
            return false;
        }

        this.directory = directory;
        return true;
    }

    @Override
    public @NotNull Set<IChannel> loadAll() throws IOException {
        Set<IChannel> channels = new HashSet<>();

        File[] files = this.directory.listFiles(File::isFile);

        if (files == null) {
            throw new IOException("Failed to load files from directory: " + this.directory.getName());
        }

        for (File current : files) {
            try {
                channels.add(this.load(current));
            } catch (InvalidStorageFileException ex) {
                Chats.getPluginLogger().warning("Found illegal storage File: " + ex.getExceptionFile().getName());

            } catch (IOException ex) {
                Chats.getPluginLogger().warning("Failed to load File: " + current.getName());
            }
        }

        return channels;
    }

    @Override
    public @NotNull IChannel load(@NotNull final File file) throws InvalidStorageFileException, IOException {
        final YamlConfiguration yamlConfig = new YamlConfiguration();

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);
        }

        final String fullName = yamlConfig.getString(PATH_FULL_NAME);

        if (fullName == null || fullName.isEmpty()) {
            throw new InvalidStorageFileException(file, "Invalid channel name! Name cannot be empty or missing.");
        }

        if (!file.getName().equals(fullName.concat(".yml"))) {
            throw new InvalidStorageFileException(file, "Invalid file name! File name do not equals with the channel "
                    + "name.");
        }

        GlobalChannel channel = new GlobalChannel(this, fullName);

        if (yamlConfig.contains(PATH_SHORT_NAME)) {
            channel._setShortName(yamlConfig.getString(PATH_SHORT_NAME));
        }

        if (yamlConfig.contains(PATH_COLOR)) {
            channel._setChatColor(this.parseColor(yamlConfig.getString(PATH_COLOR), file.getName()));
        }

        if (yamlConfig.contains(PATH_CROSS_WORLD)) {
            channel._setCrossWorld(yamlConfig.getBoolean(PATH_CROSS_WORLD));
        }

        if (yamlConfig.contains(PATH_DISTANCE)) {
            channel._setDistance(yamlConfig.getInt(PATH_DISTANCE));
        }

        if (yamlConfig.contains(PATH_PASSWORD)) {
            channel._setPassword(yamlConfig.getString(PATH_PASSWORD));
        }

        if (yamlConfig.contains(PATH_FORMAT_ANNOUNCE)) {
            channel._setAnnounceFormat(yamlConfig.getString(PATH_FORMAT_ANNOUNCE));
        }

        if (yamlConfig.contains(PATH_FORMAT_BROADCAST)) {
            channel._setBroadcastFormat(yamlConfig.getString(PATH_FORMAT_BROADCAST));
        }

        if (yamlConfig.contains(PATH_FORMAT_CHANNEL)) {
            channel._setChannelFormat(yamlConfig.getString(PATH_FORMAT_CHANNEL));
        }

        if (yamlConfig.contains(PATH_FORMAT_USE_CUSTOM)) {
            channel._setUseCustomFormat(yamlConfig.getBoolean(PATH_FORMAT_USE_CUSTOM));
        }

        this.configurations.put(fullName, yamlConfig);

        channel.getFormatter().update();

        return channel;
    }

    @Override
    public boolean update(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersistChannel()) {
            throw new IllegalArgumentException("Invalid Channel! channel must be a persist channel.");
        }

        YamlConfiguration yamlConfig;
        File file = new File(this.directory, this.getFileName(channel));

        if (this.configurations.containsKey(channel.getFullName())) {
            yamlConfig = this.configurations.get(channel.getFullName());
        } else {
            yamlConfig = new YamlConfiguration();
        }

        yamlConfig.set(PATH_FULL_NAME, channel.getFullName());
        yamlConfig.set(PATH_SHORT_NAME, channel.getShortName());
        yamlConfig.set(PATH_COLOR, channel.getChatColor().name());
        yamlConfig.set(PATH_CROSS_WORLD, channel.isCrossWorld());
        yamlConfig.set(PATH_DISTANCE, channel.getDistance());

        if (channel.hasPassword()) {
            yamlConfig.set(PATH_PASSWORD, channel.getPassword());
        } else {
            yamlConfig.set(PATH_PASSWORD, "");
        }

        yamlConfig.set(PATH_FORMAT_ANNOUNCE, channel.getAnnounceFormat());
        yamlConfig.set(PATH_FORMAT_BROADCAST, channel.getBroadcastFormat());
        yamlConfig.set(PATH_FORMAT_CHANNEL, channel.getChannelFormat());
        yamlConfig.set(PATH_FORMAT_USE_CUSTOM, channel.isUseCustomFormat());

        this.configurations.put(channel.getFullName(), yamlConfig);

        try {
            yamlConfig.save(file);
            return true;
        } catch (IOException ex) {
            Chats.getPluginLogger().warning("Failed to save channel file: " + file.getName());

            return false;
        }
    }

    @Override
    public boolean delete(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersistChannel()) {
            throw new IllegalArgumentException("Invalid Channel! channel must be a persist channel.");
        }

        File file = new File(this.directory, this.getFileName(channel));

        if (!file.exists()) {
            return false;
        }

        return file.delete();
    }

    private @NotNull String getFileName(@NotNull final IChannel channel) {
        return channel.getFullName().concat(".yml");
    }

    private @NotNull ChatColor parseColor(@NotNull final String colorName, @NotNull final String fileName) {
        try {
            return ChatColor.valueOf(colorName);
        } catch (IllegalArgumentException ex) {
            Chats.getPluginLogger().warning("Invalid color name in channel file: " + fileName);

            return ChatColor.WHITE;
        }
    }
}
