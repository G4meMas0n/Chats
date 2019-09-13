package de.g4memas0n.Chats.storages;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.channels.GlobalChannel;
import de.g4memas0n.Chats.channels.IChannel;
import org.bukkit.ChatColor;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;

/**
 * Implements the {@link IChannelStorage} Interface as a YAML File storage.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: September 13th, 2019
 */
public final class YAMLChannelStorage implements IChannelStorage {

    /**
     * the path to the full name that is used in the yaml file.
     */
    private static final String PATH_FULL_NAME = "name";

    /**
     * the path to the short name that is used in the yaml file.
     */
    private static final String PATH_SHORT_NAME = "nick";

    /**
     * the path to the color that is used in the yaml file.
     */
    private static final String PATH_COLOR = "color";

    /**
     * the path to the cross-world option that is used in the yaml file.
     */
    private static final String PATH_CROSS_WORLD = "cross-world";

    /**
     * the path to the distance that is used in the yaml file.
     */
    private static final String PATH_DISTANCE = "distance";

    /**
     * the path to the password that is used in the yaml file.
     */
    private static final String PATH_PASSWORD = "password";

    /**
     * the path to the use-custom-format option that is used in the yaml file.
     */
    private static final String PATH_FORMAT_USE_CUSTOM = "format.use-custom";

    /**
     * the path to the channel format that is used in the yaml file.
     */
    private static final String PATH_FORMAT_CHANNEL = "format.channel";

    /**
     * the path to the announce format that is used in the yaml file.
     */
    private static final String PATH_FORMAT_ANNOUNCE = "format.announce";

    /**
     * the path to the broadcast format that is used in the yaml file.
     */
    private static final String PATH_FORMAT_BROADCAST = "format.broadcast";

    private final HashMap<String, YamlConfiguration> configurations;
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
    public @NotNull IChannel load(@NotNull final String fileName) throws InvalidStorageFileException, IOException {
        return this.load(new File(this.directory, fileName));
    }

    @Override
    public @NotNull IChannel load(@NotNull final File file) throws InvalidStorageFileException, IOException {
        YamlConfiguration yamlConfig = new YamlConfiguration();

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);
        }

        final String fullName = yamlConfig.getString(PATH_FULL_NAME);
        final String shortName = yamlConfig.getString(PATH_SHORT_NAME, "");
        final String password = yamlConfig.getString(PATH_PASSWORD, "");
        final String channelFormat = yamlConfig.getString(PATH_FORMAT_CHANNEL, "");
        final String announceFormat = yamlConfig.getString(PATH_FORMAT_ANNOUNCE, "");
        final String broadcastFormat = yamlConfig.getString(PATH_FORMAT_BROADCAST, "");
        final boolean useCustomFormat = yamlConfig.getBoolean(PATH_FORMAT_USE_CUSTOM, false);
        final boolean crossWorld = yamlConfig.getBoolean(PATH_CROSS_WORLD, true);
        final int distance = yamlConfig.getInt(PATH_DISTANCE, -1);
        final ChatColor chatColor = this.parseColor(yamlConfig.getString(PATH_COLOR, ChatColor.WHITE.name()),
                file.getName());

        if (fullName == null || fullName.isEmpty()) {
            throw new InvalidStorageFileException(file, "Invalid channel name! Name cannot be empty or missing.");
        }

        if (!file.getName().equals(fullName.concat(".yml"))) {
            throw new InvalidStorageFileException(file, "Invalid file name! File name do not equals with the channel "
                    + "name.");
        }

        this.configurations.put(fullName, yamlConfig);

        return new GlobalChannel(this, fullName, shortName, password, channelFormat, announceFormat,
                broadcastFormat, useCustomFormat, crossWorld, distance, chatColor);
    }

    @Override
    public boolean update(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersistChannel()) {
            throw new IllegalArgumentException("Invalid Channel! channel must be a persist channel.");
        }

        File file = new File(this.directory, this.getFileName(channel));
        YamlConfiguration yamlConfig;

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

        yamlConfig.set(PATH_FORMAT_USE_CUSTOM, channel.isUseCustomFormat());
        yamlConfig.set(PATH_FORMAT_ANNOUNCE, channel.getAnnounceFormat());
        yamlConfig.set(PATH_FORMAT_BROADCAST, channel.getBroadcastFormat());
        yamlConfig.set(PATH_FORMAT_CHANNEL, channel.getChannelFormat());

        this.configurations.put(channel.getFullName(), yamlConfig);

        try {
            yamlConfig.save(file);
            return true;
        } catch (IOException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().warning("Failed to save channel file: " + file.getName() + ".");
            }

            return false;
        }
    }

    private @NotNull ChatColor parseColor(@NotNull final String colorName, @NotNull final String fileName) {
        try {
            return ChatColor.valueOf(colorName);
        } catch (IllegalArgumentException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().severe("Invalid color name in channel file: " + fileName + ".");
            }

            return ChatColor.WHITE;
        }
    }

    private @NotNull String getFileName(@NotNull final IChannel channel) {
        return channel.getFullName().concat(".yml");
    }
}
