package de.g4memas0n.Chats.managers;

import de.g4memas0n.Chats.Chats;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

public final class FileManager implements IFileManager {
    private static final String FILE_CONFIG_NAME = "config.yml";
    private static final String FOLDER_CHANNELS_NAME = "channels";
    private static final String FOLDER_CHATTERS_NAME = "chatters";
    private static final String FOLDER_LOGS_NAME = "logs";
    private static final String FOLDER_RESOURCES_NAME = "resources";

    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private final Chats instance;

    private final String logFilePattern;

    private final File configFile;
    private final File channelFolder;
    private final File chatterFolder;
    private final File dataFolder;
    private final File logsFolder;
    private final File resourcesFolder;

    public FileManager(@NotNull final File pluginDataFolder) throws IllegalArgumentException {
        if (!pluginDataFolder.isDirectory()) {
            throw new IllegalArgumentException("data folder must be a directory");
        }

        this.dataFolder = pluginDataFolder;

        this.instance = Chats.getInstance();

        this.configFile = new File(pluginDataFolder, FILE_CONFIG_NAME);
        this.channelFolder = new File(pluginDataFolder, FOLDER_CHANNELS_NAME);
        this.chatterFolder = new File(pluginDataFolder, FOLDER_CHATTERS_NAME);
        this.logsFolder = new File(pluginDataFolder, FOLDER_LOGS_NAME);
        this.resourcesFolder = new File(pluginDataFolder, FOLDER_RESOURCES_NAME);

        this.checkDirectory(this.dataFolder);
        this.checkDirectory(this.channelFolder);
        this.checkDirectory(this.chatterFolder);
        this.checkDirectory(this.logsFolder);
        this.checkDirectory(this.resourcesFolder);

        Date crtTime = new Date(System.currentTimeMillis());
        this.logFilePattern = this.logsFolder.getAbsolutePath() + "/" + DATE_FORMAT.format(crtTime) + "%u.log";
    }

    @Override
    public @NotNull File getConfigFile() {
        return this.configFile;
    }

    @Override
    public @NotNull File getChannelFolder() {
        return this.channelFolder;
    }

    @Override
    public @NotNull File getChatterFolder() {
        return this.chatterFolder;
    }

    @Override
    public @NotNull File getDataFolder() {
        return this.dataFolder;
    }

    @Override
    public @NotNull File getLogsFolder() {
        return this.logsFolder;
    }

    @Override
    public @NotNull File getResourcesFolder() {
        return this.resourcesFolder;
    }

    @Override
    public @NotNull String getLogFilePattern() {
        return this.logFilePattern;
    }

    private void checkDirectory(@NotNull final File folder) throws IllegalArgumentException {
        if (!folder.isDirectory()) {
            throw new IllegalArgumentException("File must be a directory!");
        }

        if (folder.mkdir()) {
            if (this.instance != null) {
                this.instance.getLogger().info("Folder '" + folder.getName()
                        + "' in data folder do not exist. Creating folder...");
            }
        }
    }
}