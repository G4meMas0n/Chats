package de.g4memas0n.Chats;

import de.g4memas0n.Chats.managers.IChannelManager;
import de.g4memas0n.Chats.managers.IChatterManager;
import de.g4memas0n.Chats.managers.IFileManager;
import de.g4memas0n.Chats.managers.IConfigManager;
import org.jetbrains.annotations.NotNull;

public interface IChats {

    /**
     * Returns the channel manager of this plugin main class.
     * @return the channel manager.
     */
    @NotNull IChannelManager getChannelManager();

    /**
     * Returns the chatter manager of this plugin main class.
     * @return the chatter manager.
     */
    @NotNull IChatterManager getChatterManager();

    /**
     * Returns the file manager of this plugin main class.
     * @return the file manager.
     */
    @NotNull IFileManager getFileManager();

    /**
     * Returns the setting manager of this plugin main class.
     * @return the setting manager.
     */
    @NotNull IConfigManager getSettingManager();
}