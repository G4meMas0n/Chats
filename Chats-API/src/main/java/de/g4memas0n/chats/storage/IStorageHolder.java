package de.g4memas0n.chats.storage;

import org.jetbrains.annotations.NotNull;

/**
 * StorageHolder Interface that provide storage methods for Objects with a persistent storage.
 *
 * @author G4meMason
 * @since Release 1.0.0
 *
 * created: January 29th, 2020
 * changed: May 19th, 2020
 */
public interface IStorageHolder {

    /**
     * Returns the storage file of this storage holder.
     * @return the storage file.
     */
    @NotNull IStorageFile getStorage();

    /**
     * Deletes the storage file for this storage holder.
     * Any errors deleting the storage file will be logged and then ignored.
     */
    void delete();

    /**
     * (Re)Loads the storage file and updates this storage holder to the new state.
     * Any errors loading the storage file will be logged and the previous state will be kept.
     */
    void load();

    /**
     * Saves the storage file of this storage holder.
     * Any errors saving the storage file will be logged and then ignored.
     */
    void save();
}
