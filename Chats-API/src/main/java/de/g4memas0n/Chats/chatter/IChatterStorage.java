package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.io.IOException;

/**
 * Chatter Storage Interface that defines a storage representation of a chatter.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 4th, 2019
 * changed: February 2nd, 2020
 */
@Deprecated
public interface IChatterStorage {

    /**
     * Loads the file of the chatter of the given player from the directory of this storage and creates a new chatter
     * with the options that are given in the file and returns this chatter.
     * @param player the player of the chatter that should be loaded.
     * @return the chatter with the given options.
     * @throws IOException Thrown when the storage file of the given player cannot be read.
     * @throws InvalidStorageFileException Thrown when the storage file of the given player is not a valid storage file.
     */
    @NotNull IChatter load(@NotNull final Player player) throws IOException, InvalidStorageFileException;

    /**
     * Saves the file of the given chatter or creates a new file for the given chatter.
     * @param chatter the chatter that should be updated or saved.
     * @return true when the file was updated or new created as result of this call.
     */
    boolean save(@NotNull final IChatter chatter);

    /**
     * Deletes the storage file of the given chatter when it exists.
     * @param chatter the chatter that should be deleted from this storage.
     * @return true when the storage file of the given chatter was deleted.
     */
    boolean delete(@NotNull final IChatter chatter);
}
