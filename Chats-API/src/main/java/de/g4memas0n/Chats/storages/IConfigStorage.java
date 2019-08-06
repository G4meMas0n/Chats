package de.g4memas0n.Chats.storages;

import de.g4memas0n.Chats.managers.IConfigManager;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public interface IConfigStorage {

    /**
     * Returns the directory where this storage should save and load all files.
     * @return the directory of this storage.
     */
    @NotNull
    File getDirectory();

    /**
     * Sets a new directory where this storage should save and load all files.
     * @param directory the new directory of this storage.
     * @return true when the directory was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given file is no directory.
     */
    boolean setDirectory(@NotNull final File directory) throws IllegalArgumentException;

    /**
     * Loads the file with the given file name from the directory of this storage and creates a new setting manager with
     * the options that are given in the file and returns this setting manager.
     * @param fileName the name of the file from the directory of this storage, that should be loaded.
     * @return the setting manager with the given options.
     * @throws InvalidStorageFileException Thrown when the name of the created channel do not equal with the file name.
     * @throws FileNotFoundException Thrown when the file with the given file name cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull
    IConfigManager load(@NotNull final String fileName) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Creates a new setting manager with the options that are given in the given file and returns this setting manager.
     * @param file the file that should be loaded.
     * @return the setting manager with the given options.
     * @throws InvalidStorageFileException Thrown when the name of the created channel do not equal with the file name.
     * @throws FileNotFoundException Thrown when the given file cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull
    IConfigManager load(@NotNull final File file) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Updates the file of the given setting manager or creates a new file for the given setting manager.
     * @param settingManager the setting manager that should be updated or saved.
     * @return true when the file was updated or new created as result of this call.
     */
    boolean update(@NotNull final IConfigManager settingManager);
}