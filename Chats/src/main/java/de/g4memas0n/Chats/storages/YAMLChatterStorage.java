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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Implements the {@link IChatterStorage} Interface as a YAML File storage.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 10th, 2019
 * last change: September 13th, 2019
 */
public final class YAMLChatterStorage implements IChatterStorage {

    /**
     * the path to the last player name that is used in the yaml file.
     */
    private static final String PATH_LAST_PLAYER_NAME = "name";

    /**
     * the path to the last active channel that is used in the yaml file.
     */
    private static final String PATH_ACTIVE_CHANNEL = "active-channel";

    /**
     * the path to the channels-list that is used in the yaml file.
     */
    private static final String PATH_CHANNELS = "channels";

    /**
     * the path to the ignores-list that is used in the yaml file.
     */
    private static final String PATH_IGNORES = "ignores";

    private final HashMap<UUID, YamlConfiguration> configurations;
    private IChannelManager channelManager;
    private File directory;

    public YAMLChatterStorage(@NotNull final File directory, @NotNull final IChannelManager channelManager)
            throws IllegalArgumentException {
        this.channelManager = channelManager;
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
    public boolean setChannelManager(@NotNull final IChannelManager manager) {
        if (this.channelManager.equals(manager)) {
            return false;
        }

        this.channelManager = manager;
        return true;
    }

    @Override
    public @NotNull IChatter load(@NotNull final Player player) throws InvalidStorageFileException, IOException {
        YamlConfiguration yamlConfig = new YamlConfiguration();
        File file = new File(this.directory, this.getFileName(player));

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);
        } catch (FileNotFoundException ex) {
            if (Chats.getInstance() != null) {
                Chats.getInstance().getLogger().warning("Failed to find chatter file " + file.getName()
                        + ". Creating new file...");
            }

            IChatter chatter = new Chatter(player, this, this.channelManager.getDefaultChannel());
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

        return new Chatter(player, this, activeChannel, channels, ignoredPlayers);
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

    private @NotNull String getFileName(@NotNull final IChatter chatter) {
        return this.getFileName(chatter.getPlayer());
    }

    private @NotNull String getFileName(@NotNull final Player player) {
        return player.getUniqueId().toString().concat(".yml");
    }
}
