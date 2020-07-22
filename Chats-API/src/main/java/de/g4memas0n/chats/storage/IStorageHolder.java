package de.g4memas0n.chats.storage;

/**
 * StorageHolder Interface that provide storage methods for Objects with a persistent storage.
 *
 * @author G4meMason
 * @since Release 1.0.0
 */
public interface IStorageHolder {

    /**
     * Deletes the storage file for this storage holder.
     *
     * <p>Any errors deleting the storage file will be logged and then ignored.</p>
     */
    void delete();

    /**
     * (Re)Loads the storage file and updates this storage holder to the new state.
     *
     * <p>Any errors loading the storage file will be logged and the previous state will be kept.</p>
     */
    void load();

    /**
     * Saves the storage file of this storage holder.
     *
     * <p>Any errors saving the storage file will be logged and then ignored.</p>
     */
    void save();
}
