package de.g4memas0n.chats;

import de.g4memas0n.chats.channel.IChannelManager;
import de.g4memas0n.chats.chatter.IChatterManager;
import de.g4memas0n.chats.messaging.IFormatter;
import de.g4memas0n.chats.storage.configuration.ISettings;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Chats Interface, that defines the main class of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChats extends Plugin {

    /**
     * Returns the channel manager of this plugin, that handle all channels.
     *
     * @return the used channel manager.
     */
    @NotNull IChannelManager getChannelManager();

    /**
     * Returns the chatter manager of this plugin, that handle all chatters.
     *
     * @return the used chatter manager.
     */
    @NotNull IChatterManager getChatterManager();

    /**
     * Returns the formatter of this plugin, used for all chat formatting actions.
     *
     * @return the used formatter.
     */
    @NotNull IFormatter getFormatter();

    /**
     * Returns the settings of this plugin.
     *
     * @return the used settings.
     */
    @NotNull ISettings getSettings();

    /**
     * Returns the chat service of this plugin, used for extended chat formatting.
     *
     * <p>Can be null when there is no registered chat service of the vault plugin.</p>
     *
     * @return the used chat service.
     */
    @Nullable Chat getChatService();

    /**
     * Sets the chat service for this plugin.
     *
     * <p>The given service can be null to remove the chat service.</p>
     *
     * @param service the by vault registered chat service.
     */
    void setChatService(@Nullable final Chat service);
}
