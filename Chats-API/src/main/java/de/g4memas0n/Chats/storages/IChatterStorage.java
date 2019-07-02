package de.g4memas0n.Chats.storages;

import de.g4memas0n.Chats.chatters.IChatter;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public interface IChatterStorage {

    /**
     * Returns the directory where this storage should save and load all files.
     * @return the directory of this storage.
     */
    @NotNull
    File getDirectory();

    /**
     * Sets a new directory where this storage should save and load all files.
     * @param directory the new directory of this storage.
     * @return true if the directory was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given file is no directory.
     */
    boolean setDirectory(@NotNull final File directory) throws IllegalArgumentException;

    /**
     * Loads the file of the chatter of the given player from the directory of this storage and creates a new chatter
     * with the options that are given in the file and returns this chatter.
     * @param player the player of the chatter that should be loaded.
     * @return the chatter with the given options.
     * @throws InvalidStorageFileException Thrown when the uuid of the created chatter do not equal with the file name.
     * @throws FileNotFoundException Thrown when the file with the given file name cannot be found.
     * @throws IOException Thrown when the file with the given file name cannot be read.
     */
    @NotNull
    IChatter load(@NotNull final Player player) throws InvalidStorageFileException, FileNotFoundException, IOException;

    /**
     * Updates the file of the given chatter or creates a new file for the given chatter.
     * @param chatter the chatter that should be updated or saved.
     * @return true if the file was updated or new created as result of this call.
     */
    boolean update(@NotNull final IChatter chatter);
}