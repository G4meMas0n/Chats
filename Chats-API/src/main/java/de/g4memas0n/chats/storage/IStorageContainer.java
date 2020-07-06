package de.g4memas0n.chats.storage;

/**
 * StorageContainer Interface that represents a container of storage holders and provides storage methods for these.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IStorageContainer {

    /**
     * (Re)Loads the storage container and their storage holders.
     */
    void load();

    /**
     * Saves the storage container and their storage holders.
     */
    void save();
}
