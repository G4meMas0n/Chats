package de.g4memas0n.Chats;

import de.g4memas0n.Chats.logger.ChatLogFileFormatter;
import de.g4memas0n.Chats.managers.*;
import de.g4memas0n.Chats.storages.InvalidStorageFileException;
import de.g4memas0n.Chats.storages.YAMLConfigStorage;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.plugin.java.annotation.dependency.SoftDependency;
import org.bukkit.plugin.java.annotation.plugin.ApiVersion;
import org.bukkit.plugin.java.annotation.plugin.Description;
import org.bukkit.plugin.java.annotation.plugin.Plugin;
import org.bukkit.plugin.java.annotation.plugin.author.Author;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

/**
 * The Plugins main class that implements the {@link IChats} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * last change: September 13th, 2019
 */
@Plugin(name = "Chats", version = "0.0.1-SNAPSHOT")
@Description("Test Description")
@Author("G4meMas0n")
@SoftDependency("Vault")
@ApiVersion(ApiVersion.Target.v1_13)
public final class Chats extends JavaPlugin implements IChats {
    private static final String PLUGIN_VAULT_NAME = "Vault";
    private static final String PLUGIN_HERO_CHAT_NAME = "HeroChat";

    private static Chats instance;

    private IChannelManager channelManager;
    private IChatterManager chatterManager;
    private IFileManager fileManager;
    private IConfigManager configManager;
    private Logger chatLogger;

    private Chat chatService;

    public static @Nullable Chats getInstance() {
        return Chats.instance;
    }

    @Override
    public void onEnable() {
        Chats.instance = this;

        super.onEnable();
    }

    @Override
    public void onDisable() {
        super.onDisable();
    }

    private void checkForHeroChat() {
        PluginManager pluginManager = this.getPluginManager();

        org.bukkit.plugin.Plugin heroChat = pluginManager.getPlugin(PLUGIN_HERO_CHAT_NAME);

        if (heroChat != null) {
            if (heroChat.isEnabled()) {
                pluginManager.disablePlugin(heroChat);
                this.getLogger().warning("Found Plugin HeroChat, trying to disable it... (Please remove HeroChat!)");
            } else {
                this.getLogger().severe("Found Plugin HeroChat, but it is disabled. Please remove HeroChat!");
            }
        }
    }

    private boolean setupCommands(@NotNull final String permissionMessage) {
        //TODO set TabExecuter and PermissionMessage for all Commands
        return false;
    }

    private boolean setupListeners() {
        //TODO register all Listeners to Bukkit/Spigot
        return false;
    }

    private boolean setupManager() {
        try {
            this.fileManager = new FileManager(this.getDataFolder());
            this.configManager = (new YAMLConfigStorage(this.fileManager.getConfigFile())).load();

            //TODO initialize and create ChannelManager and ChatterManager

            return true;
        } catch (IllegalArgumentException | InvalidStorageFileException | IOException ex) {
            this.getLogger().warning("Failed to setup manager: " + ex.getMessage());
            return false;
        }
    }

    private boolean setupLogger() {
        try {
            this.getLogger().info("Setting up chat-logging handler...");

            this.chatLogger = Logger.getLogger("CHAT");
            this.chatLogger.setParent(this.getLogger());

            if (this.configManager.isLogToConsole()) {
                this.chatLogger.setUseParentHandlers(true);
            } else {
                this.chatLogger.setUseParentHandlers(false);
            }

            if (this.configManager.isLogToFile()) {
                final FileHandler fileHandler = new FileHandler(this.fileManager.getLogFilePattern(), true);

                fileHandler.setFormatter(new ChatLogFileFormatter());

                this.chatLogger.addHandler(fileHandler);
            }

            this.getLogger().info("Chat-logging handler has been set up.");

            return true;
        } catch (IOException | SecurityException ex) {
            this.getLogger().warning("Failed to setup chat-logging handler: " + ex.getMessage());
            return false;
        }
    }

    private boolean setupChatService() {
        if (this.getPluginManager().getPlugin(PLUGIN_VAULT_NAME) == null) {
            this.chatService = null;
            return false;
        }

        this.getLogger().info("Found Plugin Vault. Setting up chat service...");
        RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

        if (rsp == null) {
            this.chatService = null;
            this.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
            return false;
        } else {
            this.chatService = rsp.getProvider();

            if (this.chatService != null) {
                this.getLogger().info("Chat service has been set up.");
                return true;
            } else {
                this.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
                return false;
            }
        }
    }

    public @Nullable Chat getChatService() {
        return this.chatService;
    }

    @Override
    public @NotNull Logger getChatLogger() {
        return this.chatLogger;
    }

    @Override
    public @NotNull PluginManager getPluginManager() {
        return this.getServer().getPluginManager();
    }

    @Override
    public @NotNull IChannelManager getChannelManager() {
        return this.channelManager;
    }

    @Override
    public @NotNull IChatterManager getChatterManager() {
        return this.chatterManager;
    }

    @Override
    public @NotNull IFileManager getFileManager() {
        return this.fileManager;
    }

    @Override
    public @NotNull IConfigManager getConfigManager() {
        return this.configManager;
    }
}
