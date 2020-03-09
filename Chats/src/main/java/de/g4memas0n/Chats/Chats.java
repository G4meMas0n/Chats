package de.g4memas0n.Chats;

import de.g4memas0n.Chats.channel.ChannelManager;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.command.ChannelCommand;
import de.g4memas0n.Chats.command.ChatsCommand;
import de.g4memas0n.Chats.command.InfoCommand;
import de.g4memas0n.Chats.messaging.Formatter;
import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.chatter.ChatterManager;
import de.g4memas0n.Chats.chatter.IChatterManager;
import de.g4memas0n.Chats.command.BasicCommand;
import de.g4memas0n.Chats.command.BroadcastCommand;
import de.g4memas0n.Chats.command.ChatCommand;
import de.g4memas0n.Chats.command.CreateCommand;
import de.g4memas0n.Chats.command.DeleteCommand;
import de.g4memas0n.Chats.command.FocusCommand;
import de.g4memas0n.Chats.command.HelpCommand;
import de.g4memas0n.Chats.command.IgnoreCommand;
import de.g4memas0n.Chats.command.JoinCommand;
import de.g4memas0n.Chats.command.LeaveCommand;
import de.g4memas0n.Chats.command.ListCommand;
import de.g4memas0n.Chats.command.ModifyCommand;
import de.g4memas0n.Chats.command.MsgCommand;
import de.g4memas0n.Chats.command.ReloadCommand;
import de.g4memas0n.Chats.command.ReplyCommand;
import de.g4memas0n.Chats.listener.BasicListener;
import de.g4memas0n.Chats.listener.ConnectionListener;
import de.g4memas0n.Chats.listener.PlayerListener;
import de.g4memas0n.Chats.listener.PluginListener;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.storage.configuration.ISettings;
import de.g4memas0n.Chats.storage.configuration.Settings;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.util.logging.ChatLogger;
import de.g4memas0n.Chats.util.logging.Log;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.FileHandler;
import java.util.logging.Handler;

/**
 * The Plugins main class, implements {@link IChats}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 26th, 2019
 * changed: March 6th, 2020
 */
public final class Chats extends JavaPlugin implements IChats {

    private final Set<BasicCommand> commands;
    private final Set<BasicListener> listeners;

    private IChannelManager channelManager;
    private IChatterManager chatterManager;
    private IFormatter formatter;
    private ISettings settings;
    private Chat chatService;

    private Messages messages;

    public Chats() {
        this.commands = new HashSet<>(20);
        this.listeners = new HashSet<>(5);

        Log.setPluginLogger(this.getLogger());
        Log.setChatLogger(new ChatLogger(this.getLogger()));
    }

    @Override
    public void onLoad() {
        this.settings = new Settings(this);

        this.setupLoggers();

        this.messages = new Messages(this.getDataFolder());
        this.messages.setLocale(this.settings.getLocale());

        this.formatter = new Formatter(this);

        this.channelManager = new ChannelManager(this);
        this.chatterManager = new ChatterManager(this);

        try {
            this.channelManager.reload();
        } catch (IOException ex) {
            this.getLogger().warning("Unable to load channels. Plugin will be disabled...");
            this.getServer().getPluginManager().disablePlugin(this);
        }
    }

    @Override
    public void onEnable() {
        if (!this.isNotLoaded()) {
            this.getLogger().warning("Plugin was not loaded. Loading it...");
            this.onLoad();
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

        this.messages.enable();

        this.setupChatService();
        this.setupListeners();
        this.setupCommands();
    }

    @Override
    public void onDisable() {
        this.listeners.forEach(BasicListener::unregister);
        this.commands.forEach(BasicCommand::unregister);

        this.messages.disable();

        this.channelManager = null;
        this.chatterManager = null;
        this.chatService = null;
        this.formatter = null;
        this.settings = null;
        this.messages = null;
    }

    @Override
    public @NotNull FileConfiguration getConfig() {
        final IStorageFile storage = this.settings.getStorageFile();

        // Should always true, because the implementation of IStorageFile extends YamlConfiguration which is a
        // FileConfiguration.
        if (storage instanceof YamlStorageFile) {
            return (YamlStorageFile) storage;
        }

        return super.getConfig();
    }

    @Override
    public void reloadConfig() {
        this.settings.load();

        this.setupLoggers();

        this.messages.setLocale(this.settings.getLocale());

        for (final BasicCommand current : this.commands) {
            current.setDescription(Messages.tl("commandDescription_" + current.getName()));
        }
    }

    @Override
    public void saveConfig() {
        this.settings.save();
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
    public @NotNull IFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public @NotNull ISettings getSettings() {
        return this.settings;
    }

    @Override
    public @Nullable Chat getChatService() {
        return this.chatService;
    }

    @Override
    public void setChatService(@Nullable final Chat service) {
        this.chatService = service;
    }

    private boolean isNotLoaded() {
        return this.channelManager == null || this.chatterManager == null || this.settings == null
                || this.messages == null;
    }

    private void setupLoggers() {
        Log.setColored(this.settings.isLogColored());
        Log.setDebug(this.settings.isLogDebug());

        Log.getChatLogger().setUseParentHandlers(this.settings.isLogToConsole());

        if (!this.settings.isLogToFile()) {
            for (final Handler current : Log.getChatLogger().getHandlers()) {
                if (current instanceof FileHandler) {
                    Log.getChatLogger().removeHandler(current);
                }
            }
        } else {
            for (final Handler current : Log.getChatLogger().getHandlers()) {
                if (current instanceof FileHandler) {
                    return;
                }
            }

            final FileHandler handler = Log.setupFileHandler(this.getDataFolder());

            if (handler != null) {
                Log.getChatLogger().addHandler(handler);
            }
        }
    }

    private void setupChatService() {
        if (this.getServer().getPluginManager().getPlugin(PluginListener.PLUGIN_NAME_VAULT) == null) {
            this.getLogger().severe("Unable to detect plugin 'Vault'! Vault integration has been disabled.");
            return;
        }

        this.getLogger().info("Detected supported plugin: 'Vault'! Setting up chat service...");
        final RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

        if (rsp == null) {
            this.getLogger().severe("Unable to setup chat service. Vault integration has been disabled.");
        } else {
            this.chatService = rsp.getProvider();
            this.getLogger().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    private void setupListeners() {
        this.listeners.add(new PluginListener());
        this.listeners.add(new ConnectionListener());
        this.listeners.add(new PlayerListener());

        this.listeners.forEach(listener -> listener.register(this));
    }

    private void setupCommands() {
        this.commands.add(new BroadcastCommand());
        this.commands.add(new ChannelCommand());
        this.commands.add(new ChatCommand());
        this.commands.add(new ChatsCommand());
        this.commands.add(new CreateCommand());
        this.commands.add(new DeleteCommand());
        this.commands.add(new FocusCommand());
        this.commands.add(new HelpCommand());
        this.commands.add(new IgnoreCommand());
        this.commands.add(new InfoCommand());
        this.commands.add(new JoinCommand());
        this.commands.add(new LeaveCommand());
        this.commands.add(new ListCommand());
        this.commands.add(new ModifyCommand());
        this.commands.add(new MsgCommand());
        this.commands.add(new ReloadCommand());
        this.commands.add(new ReplyCommand());

        this.commands.forEach(command -> command.register(this));
    }
}
