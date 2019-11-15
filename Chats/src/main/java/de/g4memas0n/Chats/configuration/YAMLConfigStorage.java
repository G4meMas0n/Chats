package de.g4memas0n.Chats.configuration;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import org.bukkit.ChatColor;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Yaml File Configuration storage of a config manager, implements the {@link IConfigStorage} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: November 14th, 2019
 */
public final class YAMLConfigStorage implements IConfigStorage {

    /**
     * the yaml configuration paths of all saved config options.
     */
    private static final String PATH_CON_FORMAT_COLOR = "conversion-format.color";
    private static final String PATH_CON_FORMAT_NORMAL = "conversion-format.normal";
    private static final String PATH_CON_FORMAT_TWITTER = "conversion-format.twitter-style";
    private static final String PATH_CON_FORMAT_USE_TWITTER = "conversion-format.use-twitter-style";
    private static final String PATH_DEFAULT_CHANNEL = "default-channel";
    private static final String PATH_DEF_FORMAT_ANNOUNCE = "default-format.announce";
    private static final String PATH_DEF_FORMAT_BROADCAST = "default-format.broadcast";
    private static final String PATH_DEF_FORMAT_CHANNEL = "default-format.channel";
    private static final String PATH_LOCALE = "locale";
    private static final String PATH_LOGS_COLORED = "logs.colored";
    private static final String PATH_LOGS_DEBUG = "logs.debug";
    private static final String PATH_LOGS_TO_CONSOLE = "logs.to-console";
    private static final String PATH_LOGS_TO_FILE = "logs.to-file";

    private final Map<IConfigManager, YamlConfiguration> configurations;

    public YAMLConfigStorage() {
        this.configurations = new HashMap<>();
    }

    @Override
    public @NotNull IConfigManager load(@NotNull final File configFile) throws InvalidStorageFileException, IOException {
        final YamlConfiguration yamlConfig = new YamlConfiguration();

        try {
            yamlConfig.load(configFile);
        } catch (InvalidConfigurationException ex) {
            throw new InvalidStorageFileException(configFile, ex);
        }

        final String defaultChannel = yamlConfig.getString(PATH_DEFAULT_CHANNEL);

        final ConfigManager manager = new ConfigManager(this, configFile, defaultChannel);

        if (yamlConfig.contains(PATH_CON_FORMAT_COLOR)) {
            manager._setConversionColor(this.parseColor(yamlConfig.getString(PATH_CON_FORMAT_COLOR),
                    configFile.getName()));
        }

        if (yamlConfig.contains(PATH_CON_FORMAT_NORMAL)) {
            manager._setConversionFormat(yamlConfig.getString(PATH_CON_FORMAT_NORMAL));
        }

        if (yamlConfig.contains(PATH_CON_FORMAT_TWITTER)) {
            manager._setTwitterStyleFormat(yamlConfig.getString(PATH_CON_FORMAT_TWITTER));
        }

        if (yamlConfig.contains(PATH_CON_FORMAT_USE_TWITTER)) {
            manager._setUseTwitterStyle(yamlConfig.getBoolean(PATH_CON_FORMAT_USE_TWITTER));
        }

        if (yamlConfig.contains(PATH_DEF_FORMAT_ANNOUNCE)) {
            manager._setDefaultAnnounceFormat(yamlConfig.getString(PATH_DEF_FORMAT_ANNOUNCE));
        }

        if (yamlConfig.contains(PATH_DEF_FORMAT_BROADCAST)) {
            manager._setDefaultBroadcastFormat(yamlConfig.getString(PATH_DEF_FORMAT_BROADCAST));
        }

        if (yamlConfig.contains(PATH_DEF_FORMAT_CHANNEL)) {
            manager._setDefaultChannelFormat(yamlConfig.getString(PATH_DEF_FORMAT_CHANNEL));
        }

        if (yamlConfig.contains(PATH_LOCALE)) {
            manager._setLocale(this.parseLocale(yamlConfig.getString(PATH_LOCALE)));
        }

        if (yamlConfig.contains(PATH_LOGS_COLORED)) {
            manager._setLogColored(yamlConfig.getBoolean(PATH_LOGS_COLORED));
        }

        if (yamlConfig.contains(PATH_LOGS_DEBUG)) {
            manager._setLogDebug(yamlConfig.getBoolean(PATH_LOGS_DEBUG));
        }

        if (yamlConfig.contains(PATH_LOGS_TO_CONSOLE)) {
            manager._setLogToConsole(yamlConfig.getBoolean(PATH_LOGS_TO_CONSOLE));
        }

        if (yamlConfig.contains(PATH_LOGS_TO_FILE)) {
            manager._setLogToFile(yamlConfig.getBoolean(PATH_LOGS_TO_FILE));
        }

        this.configurations.put(manager, yamlConfig);

        return manager;
    }

    @Override
    public boolean update(@NotNull final IConfigManager manager) {
        final File configFile = manager.getConfigFile();
        final YamlConfiguration yamlConfig = this.configurations.get(manager);

        if (!yamlConfig.contains(PATH_CON_FORMAT_COLOR)) {
            yamlConfig.set(PATH_CON_FORMAT_COLOR, manager.getConversionColor());
        }

        if (!yamlConfig.contains(PATH_CON_FORMAT_NORMAL)) {
            yamlConfig.set(PATH_CON_FORMAT_NORMAL, manager.getConversionFormat());
        }

        if (!yamlConfig.contains(PATH_CON_FORMAT_TWITTER)) {
            yamlConfig.set(PATH_CON_FORMAT_TWITTER, manager.getTwitterStyleFormat());
        }

        if (!yamlConfig.contains(PATH_CON_FORMAT_USE_TWITTER)) {
            yamlConfig.set(PATH_CON_FORMAT_USE_TWITTER, manager.isUseTwitterStyle());
        }

        if (!yamlConfig.contains(PATH_DEF_FORMAT_ANNOUNCE)) {
            yamlConfig.set(PATH_DEF_FORMAT_ANNOUNCE, manager.getDefaultAnnounceFormat());
        }

        if (!yamlConfig.contains(PATH_DEF_FORMAT_BROADCAST)) {
            yamlConfig.set(PATH_DEF_FORMAT_BROADCAST, manager.getDefaultBroadcastFormat());
        }

        if (!yamlConfig.contains(PATH_DEF_FORMAT_CHANNEL)) {
            yamlConfig.set(PATH_DEF_FORMAT_CHANNEL, manager.getDefaultChannelFormat());
        }

        if (!yamlConfig.contains(PATH_LOCALE)) {
            yamlConfig.set(PATH_LOCALE, manager.getLocale());
        }

        if (!yamlConfig.contains(PATH_LOGS_COLORED)) {
            yamlConfig.set(PATH_LOGS_COLORED, manager.isLogColored());
        }

        if (!yamlConfig.contains(PATH_LOGS_DEBUG)) {
            yamlConfig.set(PATH_LOGS_DEBUG, manager.isLogDebug());
        }

        if (!yamlConfig.contains(PATH_LOGS_TO_CONSOLE)) {
            yamlConfig.set(PATH_LOGS_TO_CONSOLE, manager.isLogToConsole());
        }

        if (!yamlConfig.contains(PATH_LOGS_TO_FILE)) {
            yamlConfig.set(PATH_LOGS_TO_FILE, manager.isLogToFile());
        }

        try {
            yamlConfig.save(configFile);
            return true;
        } catch (IOException ex) {
            Chats.getPluginLogger().warning("Failed to save the config file '" + configFile.getName() + "': "
                    + ex.getMessage());

            return false;
        }
    }

    private @NotNull Locale parseLocale(@NotNull final String locale) {
        if (!locale.isEmpty()) {
            String[] parts = locale.split("_");

            if (parts.length == 1) {
                return new Locale(parts[0]);
            } else if (parts.length == 2) {
                return new Locale(parts[0], parts[1]);
            } else if (parts.length == 3) {
                return new Locale(parts[0], parts[1], parts[2]);
            }
        }

        return Locale.ENGLISH;
    }

    private @NotNull ChatColor parseColor(@NotNull final String colorName, @NotNull final String configName) {
        try {
            return ChatColor.valueOf(colorName);
        } catch (IllegalArgumentException ex) {
            Chats.getPluginLogger().warning("Invalid conversion color name for config file ' " + configName
                    + "': " + ex.getMessage());

            return ChatColor.LIGHT_PURPLE;
        }
    }
}
