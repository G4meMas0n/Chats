package de.g4memas0n.chats;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import de.g4memas0n.chats.channel.ChannelManager;
import de.g4memas0n.chats.channel.IChannelManager;
import de.g4memas0n.chats.chatter.ChatterManager;
import de.g4memas0n.chats.chatter.IChatterManager;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.delegate.ChannelCommand;
import de.g4memas0n.chats.command.delegate.ChatsCommand;
import de.g4memas0n.chats.command.chatter.IgnoreCommand;
import de.g4memas0n.chats.command.chatter.MsgCommand;
import de.g4memas0n.chats.command.chatter.ReplyCommand;
import de.g4memas0n.chats.listener.BasicListener;
import de.g4memas0n.chats.listener.ConnectionListener;
import de.g4memas0n.chats.listener.PlayerListener;
import de.g4memas0n.chats.listener.PluginListener;
import de.g4memas0n.chats.messaging.Formatter;
import de.g4memas0n.chats.messaging.IFormatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.storage.configuration.ISettings;
import de.g4memas0n.chats.storage.configuration.Settings;
import de.g4memas0n.chats.util.logging.BasicLogger;
import de.g4memas0n.chats.util.logging.Log;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitTask;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

/**
 * The Plugins main class, implements {@link IChats}.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: July 26th, 2019
 * changed: June 22th, 2020
 */
public final class Chats extends JavaPlugin implements IChats {

    private final Set<BasicCommand> commands;
    private final Set<BasicListener> listeners;

    private ScheduledExecutorService executor;
    private IChannelManager channelManager;
    private IChatterManager chatterManager;
    private IFormatter formatter;
    private ISettings settings;
    private Chat chatService;

    private Messages messages;
    private boolean enabled;
    private boolean loaded;

    public Chats() {
        this.commands = new HashSet<>(6, 1);
        this.listeners = new HashSet<>(4, 1);

        Log.initialize(super.getLogger(), this);
    }

    @Override
    public @NotNull IChannelManager getChannelManager() {
        if (this.channelManager == null) {
            throw new IllegalStateException("Tried to get the channel manager before it was set");
        }

        return this.channelManager;
    }

    @Override
    public @NotNull IChatterManager getChatterManager() {
        if (this.chatterManager == null) {
            throw new IllegalStateException("Tried to get the chatter manager before it was set");
        }

        return this.chatterManager;
    }

    @Override
    public @NotNull IFormatter getFormatter() {
        if (this.formatter == null) {
            throw new IllegalStateException("Tried to get the formatter before it was set");
        }

        return this.formatter;
    }

    @Override
    public @NotNull ISettings getSettings() {
        if (this.settings == null) {
            throw new IllegalStateException("Tried to get the settings before it was set");
        }

        return this.settings;
    }

    public @NotNull Messages getMessages() {
        if (this.messages == null) {
            throw new IllegalStateException("Tried to get the messages before it was set");
        }

        return this.messages;
    }

    @Override
    public @Nullable Chat getChatService() {
        return this.chatService;
    }

    @Override
    public void setChatService(@Nullable final Chat service) {
        this.chatService = service;
    }

    @Override
    public void onLoad() {
        if (this.loaded) {
            this.getLogger().severe("Tried to load plugin twice. Plugin is already loaded.");
            return;
        }

        this.settings = new Settings(this);

        Log.load(this.settings);

        this.messages = new Messages(this.getDataFolder());
        this.messages.setLocale(this.settings.getLocale());

        this.formatter = new Formatter(this);

        this.channelManager = new ChannelManager(this);
        this.chatterManager = new ChatterManager(this);

        this.loaded = true;
    }

    @Override
    public void onEnable() {
        if (this.enabled) {
            this.getLogger().severe("Tried to enable plugin twice. Plugin is already enabled.");
            return;
        }

        if (!this.loaded) {
            this.getLogger().warning("Plugin was not loaded. Loading it...");
            this.onLoad();
        }

        this.setupStorageExecutor();
        this.setupChatService();

        this.channelManager.load();
        this.chatterManager.load();

        this.messages.enable();

        this.setupListeners();
        this.setupCommands();

        this.enabled = true;
    }

    @Override
    public void onDisable() {
        if (!this.enabled) {
            this.getLogger().severe("Tried to disable plugin twice. Plugin is already disabled.");
            return;
        }

        this.getLogger().debug("Unregister all plugin listeners and commands...");

        this.listeners.forEach(BasicListener::unregister);
        this.commands.forEach(BasicCommand::unregister);

        this.getLogger().debug("All plugin listeners and commands has been unregistered.");

        this.messages.disable();

        this.channelManager.save();
        this.chatterManager.save();

        try {
            this.getLogger().debug("Shutting down storage executor...");
            this.executor.shutdown();

            if (!this.executor.awaitTermination(30, TimeUnit.SECONDS)) {
                this.getLogger().warning("Storage executor got terminated after termination timeout. "
                        + "Running tasks now, that never commenced execution...");

                for (final Runnable runnable : this.executor.shutdownNow()) {
                    runnable.run();
                }
            } else {
                this.getLogger().debug("Storage executor was successfully terminated.");
            }
        } catch (InterruptedException ex) {
            this.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage executor to terminate.", ex);
        }

        Log.exit();

        this.listeners.clear();
        this.commands.clear();

        this.channelManager = null;
        this.chatterManager = null;
        this.chatService = null;
        this.formatter = null;
        this.settings = null;
        this.messages = null;

        this.enabled = false;
        this.loaded = false;
    }

    @Override
    public @NotNull BasicLogger getLogger() {
        return Log.getPlugin();
    }

    @Override
    public @NotNull FileConfiguration getConfig() {
        // Should always true, because the implementation of IStorageFile extends YamlConfiguration which is a
        // FileConfiguration.
        if (this.settings.getStorage() instanceof YamlStorageFile) {
            return (YamlStorageFile) this.settings.getStorage();
        }

        return super.getConfig();
    }

    @Override
    public void reloadConfig() {
        this.settings.load();

        Log.load(this.settings);

        this.messages.setLocale(this.settings.getLocale());

        /*
        for (final BasicCommand current : this.getCommands()) {
            current.setDescription(Messages.tl("commandDescription_" + current.getName()));
        }
        */
    }

    @Override
    public void saveConfig() {
        this.settings.save();
    }

    private void setupStorageExecutor() {
        if (this.executor != null && !this.executor.isShutdown()) {
            this.getLogger().severe("Tried to setup storage executor twice. It was already setup.");
            return;
        }

        this.getLogger().debug("Setting up storage executor...");

        this.executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactoryBuilder().setNameFormat("Storage Thread").build());

        this.getLogger().debug("Storage executor has been setup.");
    }

    private void setupChatService() {
        if (this.chatService != null) {
            this.getLogger().severe("Tried to setup chat service twice. It was already setup.");
            return;
        }

        if (this.getServer().getPluginManager().getPlugin(PluginListener.VAULT) == null) {
            this.getLogger().warning("Unable to detect plugin 'Vault'! Vault integration has been disabled.");
            return;
        }

        this.getLogger().info("Detected supported plugin: 'Vault'! Setting up chat service...");

        final RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

        if (rsp == null) {
            this.getLogger().warning("Unable to setup chat service. Vault integration has been disabled.");
        } else {
            this.chatService = rsp.getProvider();

            this.getLogger().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    private void setupListeners() {
        if (!this.listeners.isEmpty()) {
            this.getLogger().severe("Tried to register plugin listeners twice. They are already registered.");
            return;
        }

        this.getLogger().debug("Register plugin listeners...");

        this.listeners.add(new PluginListener());
        this.listeners.add(new ConnectionListener());
        this.listeners.add(new PlayerListener());

        this.listeners.forEach(listener -> listener.register(this));

        this.getLogger().debug("All plugin listeners has been registered.");
    }

    private void setupCommands() {
        if (!this.commands.isEmpty()) {
            this.getLogger().severe("Tried to register plugin commands twice. They are already registered.");
            return;
        }

        this.getLogger().debug("Register plugin commands...");

        this.commands.add(new ChannelCommand());
        this.commands.add(new ChatsCommand());
        this.commands.add(new IgnoreCommand());
        this.commands.add(new MsgCommand());
        this.commands.add(new ReplyCommand());

        this.commands.forEach(command -> command.register(this));

        this.getLogger().debug("All plugin commands has been registered.");
    }

    public @NotNull Future<?> runStorageTask(@NotNull final Runnable task) {
        return this.executor.submit(task);
    }

    public @NotNull BukkitTask runSyncTask(@NotNull final Runnable task) {
        return this.getServer().getScheduler().runTask(this, task);
    }

    public @NotNull Future<?> scheduleStorageTask(@NotNull final Runnable task) {
        return this.executor.schedule(task, this.settings.getSaveDelay(), TimeUnit.SECONDS);
    }

    public @NotNull BukkitTask scheduleSyncTask(@NotNull final Runnable task, final long delay) {
        return this.getServer().getScheduler().runTaskLater(this, task, delay);
    }
}
