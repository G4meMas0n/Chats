package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.chatter.IChatterManager;
import de.g4memas0n.Chats.storage.configuration.ISettings;
import de.g4memas0n.Chats.util.type.ReloadType;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Chats Interface, that defines the main class of this plugin.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * changed: March 5th, 2020
 */
public interface IChats extends Plugin {

    /**
     * Returns the channel manager of this plugin, that handle all channels.
     * @return the used channel manager.
     */
    @NotNull IChannelManager getChannelManager();

    /**
     * Returns the chatter manager of this plugin, that handle all chatters.
     * @return the used chatter manager.
     */
    @NotNull IChatterManager getChatterManager();

    /**
     * Returns the formatter of this plugin, used for all chat formatting actions.
     * @return the used formatter.
     */
    @NotNull IFormatter getFormatter();

    /**
     * Returns the settings of this plugin.
     * @return the used settings.
     */
    @NotNull ISettings getSettings();

    /**
     * Returns the chat service of this plugin, used for extended chat formatting. Can be null when there is no
     * registered chat service of the vault plugin.
     * @return the used chat service.
     */
    @Nullable Chat getChatService();

    /**
     * Sets the chat service for this plugin. Can be null to remove the chat service.
     * @param service the by vault registered chat service.
     */
    void setChatService(@Nullable final Chat service);
}
