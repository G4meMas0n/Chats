package de.g4memas0n.Chats.storages;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.Chatter;
import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.managers.IChannelManager;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

public final class YAMLChatterStorage implements IChatterStorage {
    private static final String PATH_LAST_PLAYER_NAME = "name";
    private static final String PATH_ACTIVE_CHANNEL = "active-channel";
    private static final String PATH_CHANNELS = "channels";
    private static final String PATH_IGNORES = "ignores";

    private final IChannelManager channelManager;
    private HashMap<UUID, YamlConfiguration> configurations;
    private File directory;

    public YAMLChatterStorage(@NotNull final File directory, @NotNull final IChannelManager channelManager)
            throws IllegalArgumentException {
        this.channelManager = channelManager;
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
    public IChatter load(@NotNull final Player player) throws InvalidStorageFileException, IOException {
        YamlConfiguration yamlConfig = new YamlConfiguration();
        File file = new File(this.directory, this.getFileName(player));

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(ex);
        } catch (FileNotFoundException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().warning("Failed to find chatter file " + file.getName()
                        + ". Creating new file...");
            }

            IChatter chatter = new Chatter(player, this.channelManager.getDefaultChannel());
            this.update(chatter);

            return chatter;
        }

        final String activeChannel = yamlConfig.getString(PATH_ACTIVE_CHANNEL);
        final Set<IChannel> channels = new HashSet<>();
        final Set<UUID> ignoredPlayers = new HashSet<>();

        for (String current : yamlConfig.getStringList(PATH_CHANNELS)) {
            if (this.channelManager.hasChannel(current)) {
                channels.add(this.channelManager.getChannel(current));
            }
        }

        if (channels.isEmpty()) {
            channels.add(this.channelManager.getDefaultChannel());
        }

        for (String current : yamlConfig.getStringList(PATH_IGNORES)) {
            ignoredPlayers.add(UUID.fromString(current));
        }

        this.configurations.put(player.getUniqueId(), yamlConfig);

        return new Chatter(player, activeChannel, channels, ignoredPlayers);
    }

    @Override
    public boolean update(@NotNull final IChatter chatter) {
        File file = new File(this.directory, this.getFileName(chatter));
        YamlConfiguration yamlConfig;

        if (this.configurations.containsKey(chatter.getPlayer().getUniqueId())) {
            yamlConfig = this.configurations.get(chatter.getPlayer().getUniqueId());
        } else {
            yamlConfig = new YamlConfiguration();
        }

        yamlConfig.set(PATH_LAST_PLAYER_NAME, chatter.getPlayer().getName());
        yamlConfig.set(PATH_ACTIVE_CHANNEL, chatter.getActiveChannel().getFullName());

        final Set<String> channels = new HashSet<>();

        for (IChannel current : chatter.getChannels()) {
            channels.add(current.getFullName());
        }

        yamlConfig.set(PATH_CHANNELS, channels);
        yamlConfig.set(PATH_IGNORES, chatter.getIgnoredChatters());

        this.configurations.put(chatter.getPlayer().getUniqueId(), yamlConfig);

        try {
            yamlConfig.save(file);
            return true;
        } catch (IOException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().warning("Failed to save chatter file: " + file.getName() + ".");
            }

            return false;
        }
    }

    @NotNull
    private String getFileName(@NotNull final IChatter chatter) {
        return this.getFileName(chatter.getPlayer());
    }

    @NotNull
    private String getFileName(@NotNull final Player player) {
        return player.getUniqueId().toString().concat(".yml");
    }
}