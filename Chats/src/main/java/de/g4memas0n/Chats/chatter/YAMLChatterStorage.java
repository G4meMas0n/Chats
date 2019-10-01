package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Yaml File Configuration storage of a chatter, implements the {@link IChatterStorage} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 10th, 2019
 * last change: October 1st, 2019
 */
public final class YAMLChatterStorage implements IChatterStorage {

    private static final String PATH_LAST_PLAYER_NAME = "last-name"; // Currently unused.
    private static final String PATH_ACTIVE_CHANNEL = "active-channel";
    private static final String PATH_CHANNELS = "channels";
    private static final String PATH_IGNORES = "ignores";

    private final Map<UUID, YamlConfiguration> configurations;
    private IChannelManager channelManager;
    private File directory;

    public YAMLChatterStorage(@NotNull final File directory,
                              @NotNull final IChannelManager channelManager) throws IllegalArgumentException {
        this.configurations = new HashMap<>();
        this.channelManager = channelManager;
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
    public @NotNull IChannelManager getChannelManager() {
        return this.channelManager;
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
    public @NotNull IChatter load(@NotNull final Player player) throws IOException, InvalidStorageFileException {
        YamlConfiguration yamlConfig = new YamlConfiguration();
        File file = new File(this.directory, this.getFileName(player));

        Chats instance = Chats.getInstance();

        Chatter chatter = new Chatter(player, this);

        try {
            yamlConfig.load(file);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);
        } catch (FileNotFoundException ex) {
            if (instance != null) {
                instance.getLogger().warning("Failed to find chatter file " + file.getName()
                        + ". Creating new file...");
            }

            chatter._setActiveChannel(this.channelManager.getDefaultChannel());
            this.update(chatter);

            return chatter;
        }

        if (yamlConfig.contains(PATH_CHANNELS)) {
            final Set<IChannel> channels = new HashSet<>();
            final Set<String> existing = new HashSet<>();

            for (String current : yamlConfig.getStringList(PATH_CHANNELS)) {
                if (!this.channelManager.hasPersistChannel(current)) {
                    continue;
                }

                channels.add(this.channelManager.getChannel(current));
                existing.add(current);
            }

            yamlConfig.set(PATH_CHANNELS, existing);
            chatter._setChannels(channels);
        }

        if (yamlConfig.contains(PATH_ACTIVE_CHANNEL)) {
            final IChannel active = this.channelManager.getChannel(yamlConfig.getString(PATH_ACTIVE_CHANNEL));

            if (active != null) {
                chatter._setActiveChannel(active);
            }
        } else {
            chatter._setActiveChannel(this.channelManager.getDefaultChannel());
        }

        if (yamlConfig.contains(PATH_IGNORES)) {
            final Set<UUID> ignores = new HashSet<>();

            for (String current : yamlConfig.getStringList(PATH_IGNORES)) {
                ignores.add(UUID.fromString(current));
            }

            chatter._setIgnores(ignores);
        }

        this.configurations.put(player.getUniqueId(), yamlConfig);

        return chatter;
    }

    @Override
    public boolean update(@NotNull final IChatter chatter) {
        YamlConfiguration yamlConfig;
        File file = new File(this.directory, this.getFileName(chatter.getPlayer()));

        if (this.configurations.containsKey(chatter.getPlayer().getUniqueId())) {
            yamlConfig = this.configurations.get(chatter.getPlayer().getUniqueId());
        } else {
            yamlConfig = new YamlConfiguration();
        }

        yamlConfig.set(PATH_LAST_PLAYER_NAME, chatter.getPlayer().getName());
        yamlConfig.set(PATH_ACTIVE_CHANNEL, chatter.getActiveChannel().getFullName());
        yamlConfig.set(PATH_CHANNELS, this.getPersistChannels(chatter.getChannels()));
        yamlConfig.set(PATH_IGNORES, this.toStringSet(chatter.getIgnores()));

        this.configurations.put(chatter.getPlayer().getUniqueId(), yamlConfig);

        try {
            yamlConfig.save(file);
            return true;
        } catch (IOException ex) {
            Chats instance = Chats.getInstance();

            if (instance != null) {
                instance.getLogger().warning("Failed to save chatter file: " + file.getName() + ".");
            }

            return false;
        }
    }

    private @NotNull String getFileName(@NotNull final Player player) {
        return player.getUniqueId().toString().concat(".yml");
    }

    private @NotNull Set<String> getPersistChannels(@NotNull final Set<IChannel> channels) {
        final Set<String> strings = new HashSet<>();

        for (IChannel current : channels) {
            if (current.isPersistChannel()) {
                strings.add(current.getFullName());
            }
        }

        return strings;
    }

    private @NotNull Set<String> toStringSet(@NotNull final Set<?> set) {
        Set<String> strings = new HashSet<>();

        for (Object current : set) {
            strings.add(current.toString());
        }

        return strings;
    }
}
