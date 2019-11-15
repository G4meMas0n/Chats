package de.g4memas0n.Chats.configuration;

import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Config Storage Interface that defines a config storage representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: November 13th, 2019
 */
public interface IConfigStorage {

    /**
     * Creates a new setting manager with the options that are given in the given file and returns this setting manager.
     * @return the setting manager with the given options.
     * @throws InvalidStorageFileException Thrown when the name of the created channel do not equal with the file name.
     * @throws FileNotFoundException Thrown when the file with the given file name cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull IConfigManager load(@NotNull final File configFile) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Updates the file of the given setting manager or creates a new file for the given setting manager.
     * @param configManager the setting manager that should be updated or saved.
     * @return true when the file was updated or new created as result of this call.
     */
    boolean update(@NotNull final IConfigManager configManager);
}
