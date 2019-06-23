package de.g4memas0n.Chats.managers;

import org.jetbrains.annotations.NotNull;
import java.io.File;

public interface IFileManager {

    /**
     * Returns the configuration file of this plugin.
     * @return the file of the configuration.
     */
    @NotNull File getConfigFile();

    /**
     * Returns the channels folder of this plugin.
     * @return the file directory of the channels folder.
     */
    @NotNull File getChannelFolder();

    /**
     * Returns the chatters folder of this plugin.
     * @return the file directory of the chatters folder.
     */
    @NotNull File getChatterFolder();

    /**
     * Returns the logs folder of this plugin.
     * @return the file directory of the logs folder.
     */
    @NotNull File getLogsFolder();

    /**
     * Returns the file pattern for the log file.
     * @return the file pattern.
     */
    @NotNull String getLogFilePattern();
}