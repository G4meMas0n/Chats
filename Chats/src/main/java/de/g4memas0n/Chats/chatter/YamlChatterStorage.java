package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannelManager;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.util.HashMap;
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
 * changed: February 2nd, 2020
 */
@Deprecated
public final class YamlChatterStorage implements IChatterStorage {

    /**
     * the folder name for the chatters directory.
     */
    private static final String DIRECTORY_NAME = "chatters";

    /**
     * the yaml configuration paths of all saved chatter options.
     */
    private static final String PATH_LAST_NAME = "last-name";
    private static final String PATH_CHANNELS = "channels";
    private static final String PATH_FOCUSED = "focused";
    private static final String PATH_IGNORES = "ignores";

    private final Map<UUID, YamlConfiguration> configurations;
    private final Map<UUID, Set<String>> lastNPChannels;
    private final Map<UUID, String> lastNPFocused;
    private final IChannelManager manager;
    private final File directory;

    public YamlChatterStorage(@NotNull final IChannelManager manager,
                              @NotNull final File parent) throws IllegalArgumentException {
        if (!parent.isDirectory()) {
            throw new IllegalArgumentException("Parent File must be a directory");
        }

        this.configurations = new HashMap<>();
        this.lastNPChannels = new HashMap<>();
        this.lastNPFocused = new HashMap<>();

        this.manager = manager;
        this.directory = new File(parent, DIRECTORY_NAME);
    }

    @Override
    public @NotNull IChatter load(@NotNull final Player player) {
        /*
        final YamlConfiguration yamlConfig = new YamlConfiguration();
        final StandardChatter chatter = new StandardChatter(player, this.manager, this.getStorageFile(player));
        final File file = this.getStorageFile(player);

        try {
            yamlConfig.load(file);

        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(file, ex);

        } catch (FileNotFoundException ex) {
            chatter.setFocus(this.manager.getDefault());

            this.save(chatter);

            return chatter;
        }

        Set<String> channels;
        IChannel channel;

        if (yamlConfig.contains(PATH_CHANNELS)) {
            channels = new HashSet<>(yamlConfig.getStringList(PATH_CHANNELS));

            for (String current : channels) {
                channel = this.manager.getChannel(current);

                if (channel == null || !channel.isPersist()) {
                    continue;
                }

                chatter.addChannel(channel);
            }
        }

        if (this.lastNPChannels.containsKey(player.getUniqueId())) {
            channels = this.lastNPChannels.get(player.getUniqueId());

            for (String current : channels) {
                channel = this.manager.getChannel(current);

                if (channel == null || channel.isConversation()) {
                    continue;
                }

                chatter.addChannel(channel);
            }
        }

        if (yamlConfig.contains(PATH_FOCUSED)) {
            final String focus = yamlConfig.getString(PATH_FOCUSED);

            if (focus != null && !focus.isEmpty()) {
                IChannel focused = this.manager.getChannel(focus);

                if (focused == null || !focused.isPersist()) {
                    focused = this.manager.getDefault();
                }

                chatter.setFocus(focused);
            }
        }

        if (this.lastNPFocused.containsKey(player.getUniqueId())) {
            channel = this.manager.getChannel(this.lastNPFocused.get(player.getUniqueId()));

            if (channel != null && !channel.isConversation()) {
                chatter.setFocus(channel);
            }
        }

        if (yamlConfig.contains(PATH_IGNORES)) {
            for (final String current : yamlConfig.getStringList(PATH_IGNORES)) {
                try {
                    chatter.addIgnores(UUID.fromString(current));
                } catch (IllegalArgumentException ignored) {

                }
            }
        }

        chatter.addChannel(this.manager.getDefault());

        this.configurations.put(player.getUniqueId(), yamlConfig);

        return chatter;
         */

        return new StandardChatter(player, this.manager, this.getStorageFile(player));
    }

    @Override
    public boolean save(@NotNull final IChatter chatter) {
        /*
        final File file = this.getStorageFile(chatter.getPlayer());
        YamlConfiguration yamlConfig = this.configurations.get(chatter.getPlayer().getUniqueId());

        if (yamlConfig == null) {
            yamlConfig = new YamlConfiguration();
        }

        Set<IChannel> channels = chatter.getChannels().stream()
                .filter(channel -> !channel.isConversation())
                .collect(Collectors.toSet());
        IChannel channel = chatter.getFocus();

        if (channel.isConversation()) {
            channel = chatter.getLastFocused() != null ? chatter.getLastFocused() : this.manager.getDefault();
        }

        this.lastNPChannels.put(chatter.getPlayer().getUniqueId(), channels.stream()
                .map(IChannel::getFullName)
                .collect(Collectors.toSet()));
        this.lastNPFocused.put(chatter.getPlayer().getUniqueId(), channel.getFullName());

        if (!channel.isPersist()) {
            channel = chatter.getLastPersist() != null ? chatter.getLastPersist() : this.manager.getDefault();
        }

        yamlConfig.set(PATH_LAST_NAME, chatter.getPlayer().getName());
        yamlConfig.set(PATH_CHANNELS, channels.stream()
                .filter(IChannel::isPersist)
                .map(IChannel::getFullName)
                .collect(Collectors.toSet()));
        yamlConfig.set(PATH_FOCUSED, channel.getFullName());
        yamlConfig.set(PATH_IGNORES, chatter.getIgnores().stream()
                .map(UUID::toString)
                .collect(Collectors.toSet()));

        this.configurations.put(chatter.getPlayer().getUniqueId(), yamlConfig);

        try {
            yamlConfig.save(file);
            return true;
        } catch (IOException ignored) {
            return false;
        }
         */

        chatter.save();
        return true;
    }

    @Override
    public boolean delete(@NotNull final IChatter chatter) {
        /*
        final File file = this.getStorageFile(chatter.getPlayer());

        if (!file.exists()) {
            return false;
        }

        if (!file.delete()) {
            return false;
        }
         */

        chatter.delete();
        return true;
    }

    private @NotNull File getStorageFile(@NotNull final Player player) {
        return new File(this.directory, player.getUniqueId().toString().concat(".yml"));
    }
}
