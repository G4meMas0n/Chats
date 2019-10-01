package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.chatter.IChatterManager;
import de.g4memas0n.Chats.util.IFileManager;
import de.g4memas0n.Chats.configuration.IConfigManager;
import org.bukkit.plugin.PluginManager;
import org.jetbrains.annotations.NotNull;
import java.util.logging.Logger;

/**
 * Chats Interface, that defines the main class of this plugin.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * last change: September 11th, 2019
 */
public interface IChats {

    /**
     * Logs the given message to the chat logger.
     * @param message the message that should be logged.
     */
    void logChat(@NotNull String message);

    /**
     * Returns the chat logger of this plugin main class.
     * @return the chat logger.
     */
    @NotNull Logger getChatLogger();

    /**
     * Returns the plugin manager of this plugin main class.
     * @return the plugin manager.
     */
    @NotNull PluginManager getPluginManager();

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
    @NotNull IConfigManager getConfigManager();
}
