package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.util.logging.Log;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * Representation of a Chatter Manager, implements the {@link IChatterManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: October 4th, 2019
 * changed: June 19th, 2020
 */
public final class ChatterManager implements IChatterManager {

    private static final String CACHE_NAME = "username-cache";
    private static final String DIRECTORY_NAME = "chatters";

    private final Map<UUID, IChatter> chatters;

    private final ChatterCache cache;
    private final IChats instance;
    private final File directory;

    public ChatterManager(@NotNull final IChats instance) {
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);

        if (this.directory.mkdirs()) {
            Log.getPlugin().debug(String.format("Directory '%s' does not exist. Creating it...", this.directory));
        }

        this.instance = instance;
        this.cache = new ChatterCache(new YamlStorageFile(this.directory, CACHE_NAME));

        this.chatters = new HashMap<>();
    }

    @Override
    public synchronized @NotNull Set<IChatter> getChatters() {
        return new HashSet<>(this.chatters.values());
    }

    @Override
    public synchronized @Nullable IChatter getChatter(@NotNull final String name) {
        final Player player = this.instance.getServer().getPlayer(name);

        if (player != null && player.isOnline()) {
            return this.getChatter(player);
        }

        final UUID uniqueId = this.cache.get(name);

        if (uniqueId != null) {
            return this.getChatter(uniqueId);
        }

        return null;
    }

    @Override
    public synchronized @Nullable IChatter getChatter(@NotNull final UUID uniqueId) {
        return this.chatters.get(uniqueId);
    }

    @Override
    public synchronized @NotNull IChatter getChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        final IChatter chatter = this.loadChatter(player);
        final Future<?> task = this.instance.runStorageTask(chatter::load);

        try {
            task.get();
        } catch (ExecutionException ex) {
            Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
        } catch (InterruptedException ex) {
            Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        return chatter;
    }

    @Override
    public synchronized @NotNull IChatter loadChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        final IChatter chatter = new StandardChatter(this.instance, player,
                new YamlStorageFile(this.directory, player.getUniqueId().toString()));

        this.chatters.put(player.getUniqueId(), chatter);
        this.instance.runStorageTask(() -> this.updateCache(chatter));

        Log.getPlugin().debug("Chatter has been added to chatter manager: " + chatter.getName());

        return chatter;
    }

    @Override
    public synchronized @NotNull IChatter unloadChatter(@NotNull final Player player) {
        final IChatter chatter = this.getChatter(player);

        for (final IChannel channel : chatter.getChannels()) {
            channel.setMember(chatter, false);
        }

        this.chatters.remove(player.getUniqueId(), chatter);

        Log.getPlugin().debug("Chatter has been removed from chatter manager: " + chatter.getName());

        return chatter;
    }

    @Override
    public synchronized boolean hasChatters() {
        return !this.chatters.isEmpty();
    }

    @Override
    public synchronized @NotNull Set<IOfflineChatter> getOfflineChatters() {
        final Set<IOfflineChatter> collection = new HashSet<>();

        for (final File file : this.directory.listFiles(File::isFile)) {
            try {
                final IStorageFile storage = new YamlStorageFile(file);

                if (storage.getName().equals(CACHE_NAME)) {
                    continue;
                }

                final UUID uniqueId = UUID.fromString(storage.getName());

                if (this.chatters.containsKey(uniqueId)) {
                    continue;
                }

                collection.add(new OfflineChatter(this.instance, storage, uniqueId));
            } catch (IllegalArgumentException ex) {
                Log.getPlugin().warning(String.format("Detected invalid storage file '%s'. Ignoring it...", file.getName()));
            }
        }

        return collection;
    }

    @Override
    public synchronized @Nullable IOfflineChatter getOfflineChatter(@NotNull final String name) {
        final Player player = this.instance.getServer().getPlayer(name);

        if (player != null) {
            return this.getOfflineChatter(player.getUniqueId());
        }

        final UUID uniqueId = this.cache.get(name);

        if (uniqueId != null) {
            return this.getOfflineChatter(uniqueId);
        }

        return null;
    }

    @Override
    public synchronized @Nullable IOfflineChatter getOfflineChatter(@NotNull final UUID uniqueId) {
        if (this.chatters.containsKey(uniqueId)) {
            return this.chatters.get(uniqueId);
        }

        final IStorageFile storage = new YamlStorageFile(this.directory, uniqueId.toString());

        if (storage.getFile().exists()) {
            final OfflineChatter offline = new OfflineChatter(this.instance, storage, uniqueId);
            final Future<?> task = this.instance.runStorageTask(offline::load);

            try {
                task.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            return new OfflineChatter(this.instance, storage, uniqueId);
        }

        return null;
    }

    @Override
    public synchronized void load() {
        if (this.cache.getStorage().getFile().exists()) {
            Log.getPlugin().debug("Loading username-cache...");

            final Future<?> task = this.instance.runStorageTask(this.cache::load);

            try {
                task.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            Log.getPlugin().debug("Username-cache has been loaded. Cleaning it up...");

            for (final String name : this.cache.getKeys()) {
                final UUID uniqueId = this.cache.get(name);

                if (uniqueId != null) {
                    final IStorageFile storage = new YamlStorageFile(this.directory, uniqueId.toString());

                    if (storage.getFile().exists()) {
                        continue;
                    }
                }

                this.cache.invalidate(name);
            }

            Log.getPlugin().debug("Username-cache has been cleaned up.");
        }

        if (!this.chatters.isEmpty()) {
            Log.getPlugin().info("Loading online chatters...");

            final Future<?> task = this.instance.runStorageTask(() -> this.chatters.values().forEach(IStorageHolder::load));

            try {
                task.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            Log.getPlugin().info("Online chatters has been loaded.");

            this.instance.runSyncTask(() -> this.chatters.values().forEach(chatter ->
                    chatter.sendMessage(Messages.tl("focusCurrent", chatter.getFocus().getColoredName()))));
        }
    }

    @Override
    public synchronized void save() {
        if (!this.chatters.isEmpty()) {
            Log.getPlugin().info("Saving online chatters...");

            final Future<?> task = this.instance.runStorageTask(() -> this.chatters.values().forEach(IStorageHolder::save));

            try {
                task.get();
            } catch (ExecutionException ex) {
                Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception.", ex);
            } catch (InterruptedException ex) {
                Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            Log.getPlugin().info("Online chatters has been saved.");
        }
    }

    private void updateCache(@NotNull final IChatter chatter) {
        final UUID cached = this.cache.get(chatter.getName());

        if (cached != null) {
            if (cached.equals(chatter.getUniqueId())) {
                return;
            }

            this.cache.invalidate(chatter.getName());
        }

        this.cache.invalidate(chatter.getUniqueId());
        this.cache.put(chatter.getName(), chatter.getUniqueId());
        this.cache.save();
    }
}
