package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;
import java.io.File;

/**
 * File Manager Interface that defines a file manager representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 2nd, 2019
 * last change: October 1st, 2019
 */
public interface IFileManager {

    /**
     * Returns the configuration file of this plugin.
     * @return the file of the configuration.
     */
    @NotNull File getConfigFile();

    /**
     * Returns the data folder of this plugin.
     * @return the file directory of the plugins data folder
     */
    @NotNull File getDataFolder();

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
     * Returns the resources folder of this plugin.
     * @return the file directory of the logs folder.
     */
    @NotNull File getResourcesFolder();

    /**
     * Returns the file pattern for the log file.
     * @return the file pattern.
     */
    @NotNull String getLogFilePattern();
}
