package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.chatter.IChatterManager;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;

/**
 * Chats Interface, that defines the main class of this plugin.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * changed: February 1st, 2020
 */
public interface IChats extends Plugin {

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
}
