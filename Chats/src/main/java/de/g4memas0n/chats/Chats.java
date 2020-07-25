package de.g4memas0n.chats;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import de.g4memas0n.chats.channel.ChannelManager;
import de.g4memas0n.chats.chatter.ChatterManager;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.chatter.IgnoreCommand;
import de.g4memas0n.chats.command.chatter.MsgCommand;
import de.g4memas0n.chats.command.chatter.ReplyCommand;
import de.g4memas0n.chats.command.chatter.UnignoreCommand;
import de.g4memas0n.chats.command.delegate.ChannelCommand;
import de.g4memas0n.chats.command.delegate.ChatsCommand;
import de.g4memas0n.chats.listener.BasicListener;
import de.g4memas0n.chats.listener.ConnectionListener;
import de.g4memas0n.chats.listener.PlayerListener;
import de.g4memas0n.chats.listener.ServerListener;
import de.g4memas0n.chats.messaging.Formatter;
import de.g4memas0n.chats.messaging.IChat;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.messaging.VaultChat;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.storage.configuration.Settings;
import de.g4memas0n.chats.util.logging.BasicLogger;
import de.g4memas0n.chats.util.logging.BasicLogger.FileFormatter;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitTask;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.FileHandler;
import java.util.logging.Level;

/**
 * The Plugins main class, implements {@link IChats}.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class Chats extends JavaPlugin implements IChats {

    private static final String DIRECTORY_LOGS = "logs";

    private final Set<BasicCommand> commands;
    private final Set<BasicListener> listeners;

    private final BasicLogger pluginLogger;
    private final BasicLogger chatLogger;

    private ScheduledExecutorService executor;

    private ChannelManager channelManager;
    private ChatterManager chatterManager;

    private Formatter formatter;
    private Messages messages;
    private Settings settings;
    private IChat chatService;

    private boolean enabled;
    private boolean loaded;

    public Chats() {
        this.commands = new HashSet<>(7, 1);
        this.listeners = new HashSet<>(4, 1);

        this.pluginLogger = new BasicLogger(super.getLogger(), "Plugin", this.getName());
        this.chatLogger = new BasicLogger(super.getLogger(), "Chat");
    }

    @Override
    public @NotNull ChannelManager getChannelManager() {
        if (this.channelManager == null) {
            throw new IllegalStateException("Tried to get the channel manager before it was set");
        }

        return this.channelManager;
    }

    @Override
    public @NotNull ChatterManager getChatterManager() {
        if (this.chatterManager == null) {
            throw new IllegalStateException("Tried to get the chatter manager before it was set");
        }

        return this.chatterManager;
    }

    @Override
    public @NotNull Formatter getFormatter() {
        if (this.formatter == null) {
            throw new IllegalStateException("Tried to get the formatter before it was set");
        }

        return this.formatter;
    }

    public @NotNull Messages getMessages() {
        if (this.messages == null) {
            throw new IllegalStateException("Tried to get the messages before it was set");
        }

        return this.messages;
    }

    @Override
    public @NotNull Settings getSettings() {
        if (this.settings == null) {
            throw new IllegalStateException("Tried to get the settings before it was set");
        }

        return this.settings;
    }

    @Override
    public @Nullable IChat getChatService() {
        return this.chatService;
    }

    @Override
    public void setChatService(@Nullable final IChat service) {
        this.chatService = service;
    }

    @Override
    public void onLoad() {
        if (this.loaded) {
            this.getLogger().severe("Tried to load plugin twice. Plugin is already loaded.");
            return;
        }

        this.settings = new Settings(this);
        this.settings.load();

        this.pluginLogger.setDebug(this.settings.isLogDebug());

        try {
            final File directory = new File(this.getDataFolder(), DIRECTORY_LOGS);

            if (directory.mkdirs()) {
                this.getLogger().debug(String.format("Directory '%s' does not exist. Creating it...", directory));
            }

            final String date = new SimpleDateFormat("yyyy-MM-dd").format(new Date(System.currentTimeMillis()));
            final String pattern = directory.getAbsolutePath() + "/" + date + "_Chat-%u.log";
            final FileHandler handler = new FileHandler(pattern, true);

            handler.setFormatter(new FileFormatter());

            this.chatLogger.setFileHandler(handler);
        } catch (IOException ex) {
            this.getLogger().warning(String.format("Unable to create file handler: %s", ex.getMessage()));
        }

        this.chatLogger.setColored(this.settings.isLogColored());
        this.chatLogger.setUseParentHandlers(this.settings.isLogToConsole());
        this.chatLogger.setUseFileHandler(this.settings.isLogToFile());

        this.messages = new Messages(this.getDataFolder(), this.getLogger());
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

        if (this.executor == null || this.executor.isShutdown()) {
            this.getLogger().debug("Setting up storage executor...");

            this.executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactoryBuilder().setNameFormat("Storage Thread").build());

            this.getLogger().debug("Storage executor has been setup.");
        }

        if (this.getServer().getPluginManager().getPlugin(ServerListener.VAULT) != null) {
            this.getLogger().info("Detected supported plugin: Vault! Setting up chat service...");

            final RegisteredServiceProvider<Chat> rsp = this.getServer().getServicesManager().getRegistration(Chat.class);

            if (rsp != null) {
                this.chatService = new VaultChat(rsp.getProvider());

                this.getLogger().info("Chat service has been setup. Vault integration has been enabled.");
            } else {
                this.getLogger().warning("Unable to setup chat service. Vault integration has been disabled.");
            }
        } else {
            this.getLogger().warning("Unable to detect supported plugin: Vault! Vault integration has been disabled.");
        }

        this.channelManager.load();
        this.chatterManager.load();

        this.messages.enable();

        if (this.commands.isEmpty()) {
            this.getLogger().debug("Setting up plugin commands...");

            this.commands.add(new ChannelCommand());
            this.commands.add(new ChatsCommand());
            this.commands.add(new IgnoreCommand());
            this.commands.add(new MsgCommand());
            this.commands.add(new ReplyCommand());
            this.commands.add(new UnignoreCommand());

            this.getLogger().debug("All plugin commands has been setup.");
        }

        if (this.listeners.isEmpty()) {
            this.getLogger().debug("Setting up plugin listeners...");

            this.listeners.add(new ServerListener());
            this.listeners.add(new ConnectionListener());
            this.listeners.add(new PlayerListener());

            this.getLogger().debug("All plugin listeners has been setup.");
        }

        this.getLogger().debug("Register all plugin commands and listeners...");

        this.commands.forEach(command -> command.register(this));
        this.listeners.forEach(listener -> listener.register(this));

        this.getLogger().debug("All plugin commands and listeners has been registered.");

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

                for (final Runnable task : this.executor.shutdownNow()) {
                    task.run();
                }
            } else {
                this.getLogger().debug("Storage executor was successfully terminated.");
            }
        } catch (InterruptedException ex) {
            this.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage executor to terminate.", ex);
        }

        if (this.pluginLogger.getFileHandler() != null) {
            this.pluginLogger.getFileHandler().close();
        }

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
        return this.pluginLogger;
    }

    public @NotNull BasicLogger getChatLogger() {
        return this.chatLogger;
    }

    @Override
    public @NotNull YamlStorageFile getConfig() {
        return this.settings.getStorage();
    }

    @Override
    public void reloadConfig() {
        this.settings.load();

        this.pluginLogger.setDebug(this.settings.isLogDebug());
        this.chatLogger.setColored(this.settings.isLogColored());
        this.chatLogger.setUseParentHandlers(this.settings.isLogToConsole());
        this.chatLogger.setUseFileHandler(this.settings.isLogToFile());

        this.messages.setLocale(this.settings.getLocale());
    }

    @Override
    public void saveConfig() {
        this.settings.save();
    }

    public @NotNull Future<?> runStorageTask(@NotNull final Runnable task) {
        return this.executor.submit(task);
    }

    public @NotNull Future<?> scheduleStorageTask(@NotNull final Runnable task) {
        return this.executor.schedule(task, this.settings.getSaveDelay(), TimeUnit.SECONDS);
    }

    public @NotNull BukkitTask runSyncTask(@NotNull final Runnable task) {
        return this.getServer().getScheduler().runTask(this, task);
    }

    public @NotNull BukkitTask scheduleSyncTask(@NotNull final Runnable task,
                                                final long delay) {
        return this.getServer().getScheduler().runTaskLater(this, task, delay);
    }
}
