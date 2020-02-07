package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.storage.InvalidStorageFileException;
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
 * changed: February 2nd, 2020
 */
@Deprecated
public interface IChannelStorage {

    /**
     * Loads all files from the directory of this storage and creates for each file a new channel with the options that
     * are given from the specific file and returns a set of all loaded channels.
     * @return the set of all loaded channels.
     * @throws IOException Thrown when the files can not be loaded from the directory.
     */
    @NotNull Set<IChannel> loadAll() throws IOException;

    /**
     * Load a persist channel with the options that are given from the given file and returns this channel.
     * @param file the file that should be loaded.
     * @return the channel with the given options.
     * @throws InvalidStorageFileException Thrown when the name of the created channel do not equal with the file name.
     * @throws FileNotFoundException Thrown when the given file cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull IChannel load(@NotNull final File file) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Creates a new persist channel with the given full name and returns this channel.
     * This method will throw a IllegalArgumentException when there is already a storage file with the given full name.
     * @param fullName the full name for the new persist channel.
     * @return the new persist channel with the given full name.
     * @throws IllegalArgumentException Thrown when there is already a file with the given full name.
     */
    @NotNull IChannel create(@NotNull final String fullName) throws IllegalArgumentException;

    /**
     * Saves the file of the given channel or creates a new file for the given channel.
     * @param channel the channel that should be updated or saved.
     * @return true when the file was updated or new created as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel is a non persist channel.
     */
    boolean save(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Deletes the file of the given channel when it exists.
     * @param channel the channel that should be deleted from this storage.
     * @return true when the channel file was deleted.
     * @throws IllegalArgumentException Thrown when the given channel is a non persist channel.
     */
    boolean delete(@NotNull final IChannel channel) throws IllegalArgumentException;
}
