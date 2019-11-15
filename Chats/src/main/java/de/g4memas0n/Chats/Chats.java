package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.chatter.IChatterManager;
import de.g4memas0n.Chats.configuration.IConfigManager;
import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import de.g4memas0n.Chats.logger.ChatLogFileFormatter;
import de.g4memas0n.Chats.configuration.YAMLConfigStorage;
import de.g4memas0n.Chats.util.ANSIColor;
import de.g4memas0n.Chats.util.FileManager;
import de.g4memas0n.Chats.util.IFileManager;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.plugin.java.annotation.dependency.SoftDependency;
import org.bukkit.plugin.java.annotation.plugin.ApiVersion;
import org.bukkit.plugin.java.annotation.plugin.Description;
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
 * last change: November 15th, 2019
 */
@org.bukkit.plugin.java.annotation.plugin.Plugin(name = "Chats", version = "0.0.1-SNAPSHOT")
@Description("Test Description")
@Author("G4meMas0n")
@SoftDependency("Vault")
@ApiVersion(ApiVersion.Target.v1_13)
public final class Chats extends JavaPlugin implements IChats {

    /**
     * the official plugin name of the plugin Vault and of the plugin HeroChat.
     */
    private static final String PLUGIN_VAULT_NAME = "Vault";
    private static final String PLUGIN_HERO_CHAT_NAME = "HeroChat";

    private static Chats instance;
    private static Logger pluginLogger;

    private IChannelManager channelManager;
    private IChatterManager chatterManager;
    private IConfigManager configManager;
    private IFileManager fileManager;
    private Logger chatLogger;
    private Chat chatService;

    public static @Nullable Chats getInstance() {
        return Chats.instance;
    }

    public static @NotNull Logger getPluginLogger() {
        return Chats.pluginLogger;
    }

    public Chats() {
        Chats.pluginLogger = this.getLogger();
    }

    @Override
    public void onLoad() {
        this.setupManagers();
    }

    @Override
    public void onEnable() {
        if (!this.isLoaded()) {
            this.getLogger().warning("Plugin not loaded successfully. Loading it again...");
            this.onLoad();
        }

        this.checkForHeroChat();
        this.setupChatService();
        this.setupListeners();
        this.setupCommands();
        this.setupLogger();

        Chats.instance = this;
    }

    @Override
    public void onDisable() {
        this.channelManager = null;
        this.chatterManager = null;
        this.configManager = null;
        this.fileManager = null;
        Chats.instance = null;
    }

    private void setupManagers() {
        try {
            this.fileManager = new FileManager(this.getDataFolder());

            final YAMLConfigStorage configStorage = new YAMLConfigStorage();
            this.configManager = configStorage.load(this.fileManager.getConfigFile());

            //TODO initialize and create ChannelManager and ChatterManager
        } catch (IllegalArgumentException | InvalidStorageFileException | IOException ex) {
            this.getLogger().warning("Failed to setup managers: " + ex.getMessage());
        }
    }

    private boolean isLoaded() {
        return this.channelManager != null && this.chatterManager != null
                && this.configManager != null
                && this.fileManager != null;
    }

    private void checkForHeroChat() {
        PluginManager pluginManager = this.getPluginManager();

        Plugin heroChat = pluginManager.getPlugin(PLUGIN_HERO_CHAT_NAME);

        if (heroChat != null) {
            if (heroChat.isEnabled()) {
                pluginManager.disablePlugin(heroChat);
                this.getLogger().warning("Found Plugin HeroChat, trying to disable it... Please remove HeroChat!");
            } else {
                this.getLogger().severe("Found Plugin HeroChat, but it is disabled. Please remove HeroChat!");
            }
        }
    }

    private void setupChatService() {
        if (this.getPluginManager().getPlugin(PLUGIN_VAULT_NAME) == null) {
            this.getLogger().severe("Plugin Vault not found. Vault integration has been disabled.");
            this.chatService = null;
            return;
        }

        this.getLogger().info("Found Plugin Vault. Setting up chat service...");
        RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

        if (rsp == null) {
            this.chatService = null;
            this.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
        } else {
            this.chatService = rsp.getProvider();

            if (this.chatService != null) {
                this.getLogger().info("Chat service has been set up. Vault integration has been enabled.");
            } else {
                this.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
            }
        }
    }

    private void setupListeners() {
        //TODO register all Listeners to Bukkit/Spigot

    }

    private void setupCommands() {
        //TODO set TabExecuter for all Commands

    }

    private void setupLogger() {
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
        } catch (IOException | SecurityException ex) {
            this.getLogger().warning("Failed to setup chat-logging handler: " + ex.getMessage());
        }
    }

    @Override
    public void logChat(@NotNull String message) {
        if (this.configManager.isLogColored()) {
            message = ANSIColor.translateBukkitColor(message);
        } else {
            message = ChatColor.stripColor(message);
        }

        this.chatLogger.info(message);
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
    public @NotNull IConfigManager getConfigManager() {
        return this.configManager;
    }

    @Override
    public @NotNull IFileManager getFileManager() {
        return this.fileManager;
    }
}
