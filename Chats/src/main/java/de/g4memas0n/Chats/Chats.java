package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.ChannelManager;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.chat.ChatLogger;
import de.g4memas0n.Chats.chatter.ChatterManager;
import de.g4memas0n.Chats.chatter.IChatterManager;
import de.g4memas0n.Chats.command.*;
import de.g4memas0n.Chats.listener.AbstractListener;
import de.g4memas0n.Chats.listener.ConnectionListener;
import de.g4memas0n.Chats.listener.PlayerListener;
import de.g4memas0n.Chats.listener.PluginListener;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.util.ConfigKey;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * The Plugins main class, implements {@link IChats}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * changed: February 4th, 2020
 */
public final class Chats extends JavaPlugin implements IChats {

    private static final String KEY_INVALID = "Detected invalid config key! path: %s, reason: %s";
    private static final String KEY_MISSING = "Detected missing config key! path: %s, default: %s";

    private static final String FILE_CONFIG = "config.yml";
    @SuppressWarnings("unused") private static final String DIRECTORY_RESOURCES = "resources";

    private static Chats instance;

    private final Set<ChatsCommand> commands;
    private final Set<AbstractListener> listeners;

    private IChannelManager channelManager;
    private IChatterManager chatterManager;
    private YamlStorageFile configuration;

    @SuppressWarnings("unused")
    public static @Nullable Chats getInstance() {
        return Chats.instance;
    }

    public Chats() {
        this.commands = new HashSet<>();
        this.listeners = new HashSet<>();
    }

    @Override
    public void onLoad() {
        try {
            this.channelManager = new ChannelManager(this.getDataFolder());

            String defName = this.getConfig().getString(ConfigKey.DEF_CHANNEL.getPath());

            if (defName == null) {
                defName = ConfigKey.DEF_CHANNEL.getDefault();
            }

            IChannel def = this.channelManager.getChannel(defName);

            if (def == null) {
                def = this.channelManager.createPersist(defName);
            }

            this.channelManager.setDefault(def);

            this.chatterManager = new ChatterManager(this.channelManager, this.getDataFolder());
        } catch (IllegalArgumentException | IOException ex) {
            this.channelManager = null;
            this.chatterManager = null;

            this.getLogger().warning("Failed to setup managers: " + ex.getMessage());
        }
    }

    @Override
    public void onEnable() {
        if (!this.isLoaded()) {
            this.getLogger().warning("Plugin not loaded successfully. Trying to load it again...");
            this.onLoad();
        }

        if (!this.isLoaded()) {
            this.getLogger().warning("Unable to load Plugin successfully. Plugin will be disabled...");
            this.getServer().getPluginManager().disablePlugin(this);
        }

        final Plugin heroChat = this.getServer().getPluginManager().getPlugin(PluginListener.PLUGIN_NAME_HERO_CHAT);

        if (heroChat != null) {
            if (heroChat.isEnabled()) {
                this.getServer().getPluginManager().disablePlugin(heroChat);
                this.getLogger().severe("Detected unsupported plugin: 'HeroChat'! Disabling it...");
            } else {
                this.getLogger().severe("Detected unsupported plugin: 'HeroChat'! Plugin already disabled.");
            }
        }

        this.channelManager.getFormatter().setChatService(this.setupChatService());
        this.channelManager.getPerformer().setChatLogger(this.setupChatLogger());

        this.setupListeners();
        this.setupCommands();

        Chats.instance = this;
    }

    @Override
    public void onDisable() {
        this.listeners.forEach(AbstractListener::unregister);
        this.commands.forEach(ChatsCommand::unregister);

        this.channelManager = null;
        this.chatterManager = null;

        Chats.instance = null;
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] args) {
        sender.sendMessage("Unknown Command"); //TODO: add localized 'command_notEnabled' message.
        return true;
    }

    @Override
    public @NotNull IChannelManager getChannelManager() {
        return this.channelManager;
    }

    @Override
    public @NotNull IChatterManager getChatterManager() {
        return this.chatterManager;
    }

    private boolean isLoaded() {
        return this.configuration != null && this.channelManager != null && this.chatterManager != null;
    }

    private @Nullable Chat setupChatService() {
        if (this.getServer().getPluginManager().getPlugin(PluginListener.PLUGIN_NAME_VAULT) == null) {
            this.getLogger().severe("Unable to detect plugin 'Vault'! Vault integration has been disabled.");
            return null;
        }

        this.getLogger().info("Detected supported plugin: 'Vault'! Setting up chat service...");
        RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

        if (rsp == null) {
            this.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
            return null;
        } else {
            final Chat service = rsp.getProvider();
            this.getLogger().info("Chat service has been set up. Vault integration has been enabled.");
            return service;
        }
    }

    private @NotNull Logger setupChatLogger() {
        this.getLogger().info("Setting up Chat-Logger...");

        final Logger logger = new ChatLogger(this.getLogger(), this.getConfig().getBoolean(ConfigKey.LOG_COLORED.getPath()));

        logger.setUseParentHandlers(this.getConfig().getBoolean(ConfigKey.LOG_TO_CONSOLE.getPath()));

        if (this.getConfig().getBoolean(ConfigKey.LOG_TO_FILE.getPath())) {
            try {
                logger.addHandler(ChatLogger.getFileHandler(this.getDataFolder()));
            } catch (IllegalArgumentException | IOException ex) {
                this.getLogger().warning("Failed to create file handler for file logging. Using only console logging...");

                logger.setUseParentHandlers(true);
            }
        }

        this.getLogger().info("Chat-Logger has been set up.");
        return logger;
    }

    private void setupListeners() {
        this.listeners.add(new PluginListener());
        this.listeners.add(new ConnectionListener());
        this.listeners.add(new PlayerListener());

        this.listeners.forEach(listener -> listener.register(this));
    }

    private void setupCommands() {
        this.commands.add(new ChatCommand());
        this.commands.add(new FocusCommand());
        this.commands.add(new IgnoreCommand());
        this.commands.add(new JoinCommand());
        this.commands.add(new LeaveCommand());
        this.commands.add(new ModifyCommand());
        this.commands.add(new MsgCommand());
        this.commands.add(new ReloadCommand());
        this.commands.add(new ReplyCommand());

        //TODO set TabExecutor for all Commands

        this.commands.forEach(command -> command.register(this));
    }

    private void update() {
        final String permissionMsg = "Permission Message"; //TODO: Add localized 'command_permissionMessage' message.

        try {
            this.channelManager.setDefault(this.getDefaultChannel());
        } catch (IllegalArgumentException ex) {
            this.getLogger().severe(String.format(KEY_INVALID, ConfigKey.DEF_CHANNEL.getPath(), ex.getMessage()));
        }

        try {
            this.channelManager.getFormatter().setAnnounceFormat(this.getFormat(ConfigKey.FORMAT_ANNOUNCE));
        } catch (IllegalArgumentException ex) {
            this.getLogger().severe(String.format(KEY_INVALID, ConfigKey.FORMAT_ANNOUNCE.getPath(), ex.getMessage()));
        }

        try {
            this.channelManager.getFormatter().setBroadcastFormat(this.getFormat(ConfigKey.FORMAT_BROADCAST));
        } catch (IllegalArgumentException ex) {
            this.getLogger().severe(String.format(KEY_INVALID, ConfigKey.FORMAT_BROADCAST.getPath(), ex.getMessage()));
        }

        try {
            this.channelManager.getFormatter().setChatFormat(this.getFormat(ConfigKey.FORMAT_CHAT));
        } catch (IllegalArgumentException ex) {
            this.getLogger().severe(String.format(KEY_INVALID, ConfigKey.FORMAT_CHAT.getPath(), ex.getMessage()));
        }

        try {
            this.channelManager.getFormatter().setConversationFormat(this.getFormat(ConfigKey.FORMAT_CONVERSATION));
        } catch (IllegalArgumentException ex) {
            this.getLogger().severe(String.format(KEY_INVALID, ConfigKey.FORMAT_CONVERSATION.getPath(), ex.getMessage()));
        }

        this.channelManager.getFormatter().setChatColor(this.getChatColor(ConfigKey.COLOR_CHANNEL));
        this.channelManager.getFormatter().setConversationColor(this.getChatColor(ConfigKey.COLOR_CONVERSATION));
        this.channelManager.getPerformer().setChatLogger(this.setupChatLogger());

        for (final ChatsCommand current : this.commands) {
            current.setPermissionMessage(permissionMsg);
            current.setDescription("Description"); //TODO: Add localized 'command_<cmd>_description' message.
            current.setUsage("Usage"); //TODO: Add localized 'command_<cmd>_usage' message.
        }
    }

    private @NotNull ChatColor getChatColor(@NotNull final ConfigKey<ChatColor> key) {
        ChatColor color = this.getConfig().getChatColor(key.getPath());

        if (color == null) {
            final String value = this.getConfig().getString(key.getPath());

            if (value == null) {
                this.getLogger().severe(String.format(KEY_MISSING, key.getPath(), key.getDefault().name()));
            } else {
                this.getLogger().severe(String.format(KEY_INVALID, key.getPath(), "invalid chat color name"));
            }

            color = key.getDefault();
        }

        return color;
    }

    private @NotNull IChannel getDefaultChannel() {
        String value = this.getConfig().getString(ConfigKey.DEF_CHANNEL.getPath());

        if (value == null) {
            this.getLogger().severe(String.format(KEY_MISSING, ConfigKey.DEF_CHANNEL.getPath(),
                    ConfigKey.DEF_CHANNEL.getDefault()));
            value = ConfigKey.DEF_CHANNEL.getDefault();
        }

        IChannel channel = this.channelManager.getChannel(value);

        if (channel == null) {
            channel = this.channelManager.createPersist(value);
        }

        return channel;
    }

    private @NotNull String getFormat(@NotNull final ConfigKey<String> key) {
        String value = this.getConfig().getString(key.getPath());

        if (value == null) {
            this.getLogger().severe(String.format(KEY_MISSING, key.getPath(), key.getDefault()));
            value = key.getDefault();
        }

        return value;
    }

    @Override
    public @NotNull YamlStorageFile getConfig() {
        if (this.configuration == null) {
            this.reloadConfig();
        }

        return this.configuration;
    }

    @Override
    public void reloadConfig() {
        if (this.configuration == null) {
            this.configuration = new YamlStorageFile(new File(this.getDataFolder(), FILE_CONFIG));
        }

        try {
            this.configuration.load();

            if (this.isLoaded()) {
                this.update();
            }
        } catch (IOException | InvalidStorageFileException ex) {
            this.getLogger().severe("Unable to load config file '" + FILE_CONFIG + "': " + ex.getMessage() + ".");
            this.getLogger().severe("Using default configuration...");
        }
    }

    public void saveConfig() {
        if (this.configuration == null) {
            return;
        }

        try {
            this.configuration.save();
        } catch (IOException ex) {
            this.getLogger().warning("Failed to save config file '" + FILE_CONFIG + "': " + ex.getMessage() + ".");
        }
    }
}
