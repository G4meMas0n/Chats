package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import org.bukkit.ChatColor;
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
 * changed: February 2nd, 2020
 */
@Deprecated
public final class YamlChannelStorage implements IChannelStorage {

    /**
     * the folder name for the channels directory.
     */
    private static final String DIRECTORY_NAME = "channels";

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
    private final IChatFormatter formatter;
    private final IChatPerformer performer;
    private final File directory;

    public YamlChannelStorage(@NotNull final IChatFormatter formatter,
                              @NotNull final IChatPerformer performer,
                              @NotNull final File parent) throws IllegalArgumentException {
        if (!parent.isDirectory()) {
            throw new IllegalArgumentException("Parent File must be a directory");
        }

        this.configurations = new HashMap<>();
        this.formatter = formatter;
        this.performer = performer;
        this.directory = new File(parent, DIRECTORY_NAME);
    }

    @Override
    public @NotNull Set<IChannel> loadAll() throws IOException {
        final Set<IChannel> channels = new HashSet<>();
        final File[] files = this.directory.listFiles(File::isFile);

        if (files == null) {
            throw new IOException("Failed to load files from directory " + this.directory.getName());
        }

        for (final File current : files) {
            try {
                channels.add(this.load(current));
            } catch (IllegalArgumentException ignored) {

            }
        }

        return channels;
    }

    @Override
    public @NotNull IChannel load(@NotNull final File file) throws IllegalArgumentException {
        /*
        final YamlConfiguration yamlConfig = new YamlConfiguration();

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);
        }

        final String fullName = yamlConfig.getString(PATH_FULL_NAME);

        if (fullName == null || fullName.isEmpty()) {
            throw new InvalidStorageFileException(file, "Missing or empty channel full name");
        }

        if (!file.equals(this.getStorageFile(fullName))) {
            throw new InvalidStorageFileException(file, "Invalid storage file name");
        }

        PersistChannel channel = new PersistChannel(this.formatter, this.performer, file);

        if (yamlConfig.contains(PATH_SHORT_NAME)) {
            channel.setShortName(yamlConfig.getString(PATH_SHORT_NAME));
        }

        if (yamlConfig.contains(PATH_COLOR)) {
            channel.setChatColor(this.parseColor(yamlConfig.getString(PATH_COLOR)));
        }

        if (yamlConfig.contains(PATH_CROSS_WORLD)) {
            channel.setCrossWorld(yamlConfig.getBoolean(PATH_CROSS_WORLD));
        }

        if (yamlConfig.contains(PATH_DISTANCE)) {
            channel.setDistance(yamlConfig.getInt(PATH_DISTANCE));
        }

        if (yamlConfig.contains(PATH_PASSWORD)) {
            channel.setPassword(yamlConfig.getString(PATH_PASSWORD));
        }

        if (yamlConfig.contains(PATH_FORMAT_ANNOUNCE)) {
            channel.setAnnounceFormat(yamlConfig.getString(PATH_FORMAT_ANNOUNCE));
        }

        if (yamlConfig.contains(PATH_FORMAT_BROADCAST)) {
            channel.setBroadcastFormat(yamlConfig.getString(PATH_FORMAT_BROADCAST));
        }

        if (yamlConfig.contains(PATH_FORMAT_CHANNEL)) {
            channel.setChatFormat(yamlConfig.getString(PATH_FORMAT_CHANNEL));
        }

        if (yamlConfig.contains(PATH_FORMAT_USE_CUSTOM)) {
            channel.setUseCustomFormat(yamlConfig.getBoolean(PATH_FORMAT_USE_CUSTOM));
        }

        this.configurations.put(fullName, yamlConfig);
         */

        PersistChannel channel = new PersistChannel(this.formatter, this.performer, file);

        channel.reload();

        return channel;
    }

    @Override
    public @NotNull IChannel create(@NotNull final String fullName) throws IllegalArgumentException {
        final File file = this.getStorageFile(fullName);

        if (file.exists()) {
            throw new IllegalArgumentException("File named as channel file already exists");
        }

        final PersistChannel channel = new PersistChannel(this.formatter, this.performer, file);

        /*
        this.save(channel);
         */

        channel.save();

        return channel;
    }

    @Override
    public boolean save(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException("Invalid channel! channel must be persistent");
        }

        /*
        final File file = this.getStorageFile(channel.getFullName());
        YamlConfiguration yamlConfig = this.configurations.get(channel.getFullName());

        if (yamlConfig == null) {
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
        yamlConfig.set(PATH_FORMAT_CHANNEL, channel.getChatFormat());
        yamlConfig.set(PATH_FORMAT_USE_CUSTOM, channel.isUseCustomFormat());

        this.configurations.put(channel.getFullName(), yamlConfig);

        try {
            yamlConfig.save(file);

            if (this.logDebug) {
                this.logger.info("ChannelStorage: Saved storage file '" + file + "' of channel " + channel);
            }

            return true;
        } catch (IOException ex) {
            if (this.logDebug) {
                this.logger.warning("ChannelStorage: Failed to save storage file: '" + file + "' of channel " + channel);
            } else {
                this.logger.warning("Failed to save storage file '" + file.getName() + "' of channel '"
                        + channel.getFullName() + "'.");
            }

            return false;
        }
         */

        channel.save();
        return true;
    }

    @Override
    public boolean delete(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException("Invalid channel! channel must be persistent");
        }

        /*
        final File file = this.getStorageFile(channel.getFullName());

        if (!file.exists()) {
            return false;
        }

        if (!file.delete()) {
            return false;
        }
         */

        channel.delete();
        return true;
    }

    private @NotNull File getStorageFile(@NotNull final String name) {
        return new File(this.directory, name + ".yml");
    }

    private @NotNull ChatColor parseColor(@NotNull final String colorName) {
        try {
            return ChatColor.valueOf(colorName);
        } catch (IllegalArgumentException ex) {
            return ChatColor.WHITE;
        }
    }
}
