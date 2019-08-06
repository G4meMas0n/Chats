package de.g4memas0n.Chats;

import de.g4memas0n.Chats.logger.ChatLogFileFilter;
import de.g4memas0n.Chats.logger.ChatLogFileFormatter;
import de.g4memas0n.Chats.managers.*;
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
import java.util.logging.Level;

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
    private IConfigManager settingManager;

    private Chat chatService;

    @Nullable
    public static Chats getInstance() {
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

            return true;
        } catch (IllegalArgumentException ex) {
            this.getLogger().warning("Failed to setup manager: " + ex.getMessage());
            return false;
        }
    }

    private boolean setupLogger() {
        try {
            this.getLogger().info("Setting up chat-logging handler...");
            this.getLogger().setLevel(Level.FINE);

            if (this.settingManager.isLogToFile()) {
                final FileHandler fileHandler = new FileHandler(this.fileManager.getLogFilePattern(), true);

                fileHandler.setFilter(new ChatLogFileFilter());
                fileHandler.setFormatter(new ChatLogFileFormatter());

                this.getLogger().addHandler(fileHandler);
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

    @Nullable
    public Chat getChatService() {
        return this.chatService;
    }

    @NotNull
    public PluginManager getPluginManager() {
        return this.getServer().getPluginManager();
    }

    @Override
    @NotNull
    public IChannelManager getChannelManager() {
        return this.channelManager;
    }

    @Override
    @NotNull
    public IChatterManager getChatterManager() {
        return this.chatterManager;
    }

    @Override
    @NotNull
    public IFileManager getFileManager() {
        return this.fileManager;
    }

    @Override
    @NotNull
    public IConfigManager getSettingManager() {
        return this.settingManager;
    }
}