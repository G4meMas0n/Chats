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

public final class YAMLChannelStorage implements IChannelStorage {
    private static final String PATH_FULL_NAME = "name";
    private static final String PATH_SHORT_NAME = "nick";
    private static final String PATH_COLOR = "color";
    private static final String PATH_CROSS_WORLD = "cross-world";
    private static final String PATH_DISTANCE = "distance";
    private static final String PATH_PASSWORD = "password";
    private static final String PATH_FORMAT_USE_CUSTOM = "format.use-custom";
    private static final String PATH_FORMAT_CHANNEL = "format.channel";
    private static final String PATH_FORMAT_ANNOUNCE = "format.announce";
    private static final String PATH_FORMAT_BROADCAST = "format.broadcast";

    private HashMap<String, YamlConfiguration> configurations;
    private File directory;

    public YAMLChannelStorage(@NotNull final File directory) throws IllegalArgumentException {
        this.configurations = new HashMap<>();
        this.setDirectory(directory);
    }

    @Override
    @NotNull
    public File getDirectory() {
        return this.directory;
    }

    @Override
    public boolean setDirectory(@NotNull final File directory) throws IllegalArgumentException {
        if (directory.isDirectory()) {
            if (this.directory.equals(directory)) {
                return false;
            }

            this.directory = directory;
            return true;
        } else {
            throw new IllegalArgumentException("Invalid File! File must be a directory!");
        }
    }

    @Override
    @NotNull
    public IChannel load(@NotNull final String fileName) throws InvalidStorageFileException, IOException {
        return this.load(new File(this.directory, fileName));
    }

    @Override
    @NotNull
    public IChannel load(@NotNull final File file) throws InvalidStorageFileException, IOException {
        YamlConfiguration yamlConfig = new YamlConfiguration();

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(ex);
        }

        final String fullName = yamlConfig.getString(PATH_FULL_NAME);
        final String shortName = yamlConfig.getString(PATH_SHORT_NAME, "");
        final String password = yamlConfig.getString(PATH_PASSWORD, "");
        final String channelFormat = yamlConfig.getString(PATH_FORMAT_CHANNEL, IChannel.DEFAULT_CHANNEL_FORMAT);
        final String announceFormat = yamlConfig.getString(PATH_FORMAT_ANNOUNCE, IChannel.DEFAULT_ANNOUNCE_FORMAT);
        final String broadcastFormat = yamlConfig.getString(PATH_FORMAT_BROADCAST, IChannel.DEFAULT_BROADCAST_FORMAT);
        final boolean useCustomFormat = yamlConfig.getBoolean(PATH_FORMAT_USE_CUSTOM, false);
        final boolean crossWorld = yamlConfig.getBoolean(PATH_CROSS_WORLD, true);
        final int distance = yamlConfig.getInt(PATH_DISTANCE, -1);
        final ChatColor chatColor = this.parseColor(yamlConfig.getString(PATH_COLOR, ChatColor.WHITE.name()),
                file.getName());

        if (fullName.isEmpty()) {
            throw new InvalidStorageFileException("Invalid channel name! Name cannot be empty.");
        }

        if (!file.getName().equals(fullName.concat(".yml"))) {
            throw new InvalidStorageFileException("Invalid file name! File name do not equals with the channel name.");
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
        yamlConfig.set(PATH_FORMAT_CHANNEL, channel.getCustomChannelFormat());
        yamlConfig.set(PATH_FORMAT_ANNOUNCE, channel.getCustomAnnounceFormat());
        yamlConfig.set(PATH_FORMAT_BROADCAST, channel.getCustomBroadcastFormat());

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

    @NotNull
    private ChatColor parseColor(@NotNull final String colorName, @NotNull final String fileName) {
        try {
            return ChatColor.valueOf(colorName);
        } catch (IllegalArgumentException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().severe("Invalid color name in channel file: " + fileName + ".");
            }

            return ChatColor.WHITE;
        }
    }

    @NotNull
    private String getFileName(@NotNull final IChannel channel) {
        return channel.getFullName().concat(".yml");
    }
}