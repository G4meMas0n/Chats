package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Set;

/**
 * Channel Storage Interface that defines a storage representation for persist channel.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 4th, 2019
 * last change: October 1st, 2019
 */
public interface IChannelStorage {

    /**
     * Returns the directory where this storage should save and load all files.
     * @return the directory of this storage.
     */
    @NotNull File getDirectory();

    /**
     * Sets a new directory where this storage should save and load all files.
     * @param directory the new directory of this storage.
     * @return true when the directory was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given file is no directory.
     */
    boolean setDirectory(@NotNull final File directory) throws IllegalArgumentException;

    /**
     * Loads all files from the directory of this storage and creates for each file a new channel with the options that
     * are given from the specific file and returns a set of all loaded channels.
     * @return the set of all loaded channels.
     * @throws IOException Thrown when the files can not be loaded from the directory.
     */
    @NotNull Set<IChannel> loadAll() throws IOException;

    /**
     * Creates a new Channel with the options that are given in the given file and returns this channel.
     * @param file the file that should be loaded.
     * @return the channel with the given options.
     * @throws InvalidStorageFileException Thrown when the name of the created channel do not equal with the file name.
     * @throws FileNotFoundException Thrown when the given file cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull IChannel load(@NotNull final File file) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Updates the file of the given channel or creates a new file for the given channel.
     * @param channel the channel that should be updated or saved.
     * @return true when the file was updated or new created as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is non persist channel.
     */
    boolean update(@NotNull final IChannel channel) throws IllegalArgumentException;
}
