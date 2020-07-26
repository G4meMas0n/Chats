package de.g4memas0n.chats.chatter;

import com.google.common.collect.MapMaker;
import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.storage.cache.UniqueIdCache;
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
import java.util.logging.Level;

/**
 * Implementation of a chatter manager, that holds and manage currently online chatters.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChatterManager implements IChatterManager {

    private static final String CACHE_NAME = "unique-id-cache";
    private static final String DIRECTORY_NAME = "chatters";

    private final Map<UUID, StandardChatter> chatters;
    private final Map<UUID, OfflineChatter> offlines;

    private final UniqueIdCache cache;

    private final Chats instance;
    private final File directory;

    public ChatterManager(@NotNull final Chats instance) {
        this.instance = instance;
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);

        if (this.directory.mkdirs()) {
            this.instance.getLogger().debug(String.format("Directory '%s' does not exist. Creating it...", this.directory));
        }

        this.cache = new UniqueIdCache(new YamlStorageFile(this.directory, CACHE_NAME), instance.getLogger());

        this.chatters = new HashMap<>();
        this.offlines = new MapMaker().weakValues().makeMap();
    }

    @Override
    public synchronized @NotNull Set<IChatter> getChatters() {
        return new HashSet<>(this.chatters.values());
    }

    @Override
    public synchronized @Nullable StandardChatter getChatter(@NotNull final String name) {
        final Player player = this.instance.getServer().getPlayerExact(name);

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
    public synchronized @Nullable StandardChatter getChatter(@NotNull final UUID uniqueId) {
        return this.chatters.get(uniqueId);
    }

    @Override
    public synchronized @NotNull StandardChatter getChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        final StandardChatter chatter = this.loadChatter(player);

        try {
            this.instance.runStorageTask(chatter::load).get();
        } catch (ExecutionException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
        } catch (InterruptedException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        return chatter;
    }

    @Override
    public synchronized @NotNull StandardChatter loadChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        final YamlStorageFile storage = new YamlStorageFile(this.directory, player.getUniqueId().toString());
        final StandardChatter chatter = new StandardChatter(this.instance, storage, player);

        this.chatters.put(player.getUniqueId(), chatter);
        this.offlines.remove(player.getUniqueId());

        this.instance.runStorageTask(() -> this.cache.update(player.getName(), player.getUniqueId()));

        return chatter;
    }

    @Override
    public synchronized @NotNull StandardChatter unloadChatter(@NotNull final Player player) {
        final StandardChatter chatter = this.chatters.remove(player.getUniqueId());

        if (chatter == null) {
            final YamlStorageFile storage = new YamlStorageFile(this.directory, player.getUniqueId().toString());

            return new StandardChatter(this.instance, storage, player);
        }

        for (final IChannel channel : chatter.getChannels()) {
            channel.removeMember(chatter, true);
        }

        return chatter;
    }

    @Override
    public synchronized boolean hasChatters() {
        return !this.chatters.isEmpty();
    }

    @Override
    public synchronized @NotNull Set<IOfflineChatter> getOfflineChatters() {
        final Set<IOfflineChatter> offlines = new HashSet<>();

        for (final File file : this.directory.listFiles(File::isFile)) {
            try {
                final UUID uniqueId = UUID.fromString(file.getName().substring(0, file.getName().lastIndexOf(".")));

                if (this.chatters.containsKey(uniqueId)) {
                    continue;
                }

                if (this.offlines.containsKey(uniqueId)) {
                    offlines.add(this.offlines.get(uniqueId));
                }

                final OfflineChatter offline = new OfflineChatter(this.instance, new YamlStorageFile(file), uniqueId);

                this.offlines.put(uniqueId, offline);

                offlines.add(offline);
            } catch (IllegalArgumentException ignored) {
                // Directory can contain invalid storage files, just ignore them.
            }
        }

        return offlines;
    }

    @Override
    public synchronized @Nullable IOfflineChatter getOfflineChatter(@NotNull final String name) {
        final Player player = this.instance.getServer().getPlayerExact(name);

        if (player != null && player.isOnline()) {
            return this.getChatter(player);
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

        if (this.offlines.containsKey(uniqueId)) {
            return this.offlines.get(uniqueId);
        }

        final YamlStorageFile storage = new YamlStorageFile(this.directory, uniqueId.toString());

        if (storage.getFile().exists()) {
            final OfflineChatter offline = new OfflineChatter(this.instance, storage, uniqueId);

            this.offlines.put(uniqueId, offline);

            try {
                this.instance.runStorageTask(offline::load).get();
            } catch (ExecutionException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
            } catch (InterruptedException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            return offline;
        }

        return null;
    }

    @Override
    public synchronized void load() {
        if (this.cache.exists()) {
            this.instance.getLogger().debug("Loading unique-id cache...");

            try {
                this.instance.runStorageTask(this.cache::load).get();
            } catch (ExecutionException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
            } catch (InterruptedException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            this.instance.getLogger().debug("Unique-id cache has been loaded.");

            if (this.cache.size() >= this.directory.listFiles(File::isFile).length) {
                this.instance.getLogger().debug("Detected too many entries in unique-id cache. Cleaning it up...");

                for (final Map.Entry<String, UUID> entry : this.cache.getAll().entrySet()) {
                    final IStorageFile storage = new YamlStorageFile(this.directory, entry.getValue().toString());

                    if (!storage.getFile().exists()) {
                        this.cache.invalidate(entry.getKey());
                    }
                }

                this.instance.getLogger().debug("Unique-id cache has been cleaned up. Saving it...");
                this.instance.runStorageTask(this.cache::save);
            }
        }

        if (!this.chatters.isEmpty()) {
            this.instance.getLogger().info("Loading online chatters...");

            try {
                this.instance.runStorageTask(() -> this.chatters.values().forEach(IStorageHolder::load)).get();
            } catch (ExecutionException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
            } catch (InterruptedException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            this.instance.getLogger().info("Online chatters has been loaded.");

            if (this.instance.getSettings().isInform()) {
                this.instance.scheduleSyncTask(() -> this.chatters.values().forEach(chatter -> chatter.sendMessage(
                        Messages.tl("focusCurrent", chatter.getFocus().getColoredName()))),
                        this.instance.getSettings().getInformDelay());
            }
        }
    }

    @Override
    public synchronized void save() {
        if (!this.chatters.isEmpty()) {
            this.instance.getLogger().info("Saving online chatters...");

            try {
                this.instance.runStorageTask(() -> this.chatters.values().forEach(IStorageHolder::save)).get();
            } catch (ExecutionException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
            } catch (InterruptedException ex) {
                this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
            }

            this.instance.getLogger().info("Online chatters has been saved.");
        }
    }
}
