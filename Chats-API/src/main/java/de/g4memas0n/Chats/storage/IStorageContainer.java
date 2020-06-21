package de.g4memas0n.chats.storage;

/**
 * StorageContainer Interface that represents a container of storage holders and provides storage methods for these.
 *
 * @author G4meMas0n
 * @since 0.2.2-SNAPSHOT
 *
 * created: June 9th, 2020
 * changed: June 9th, 2020
 */
public interface IStorageContainer {

    /*
     * Clears the storage container.
     */
    //void clear();

    /**
     * (Re)Loads the storage container and their storage holders.
     */
    void load();

    /**
     * Saves the storage container and their storage holders.
     */
    void save();
}
