package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.exception.InvalidStorageFileException;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Representation of a Chatter Manager, implements the {@link IChatterManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: October 4th, 2019
 * last change: November 14th, 2019
 */
public final class ChatterManager implements IChatterManager {

    private final Map<UUID, IChatter> chatters;
    private final IChatterStorage storage;

    public ChatterManager(@NotNull final File directory, @NotNull final IChannelManager manager) {
        this.chatters = new HashMap<>();
        this.storage = new YAMLChatterStorage(directory, manager);
    }

    @Override
    public @NotNull IChatterStorage getChatterStorage() {
        return this.storage;
    }

    @Override
    public boolean hasChatter(@NotNull final Player player) {
        return this.chatters.containsKey(player.getUniqueId());
    }

    @Override
    public boolean hasChatter(@NotNull final UUID uuid) {
        return this.chatters.containsKey(uuid);
    }

    @Override
    public @NotNull Set<IChatter> getChatters() {
        return new HashSet<>(this.chatters.values());
    }

    @Override
    public @Nullable IChatter getChatter(@NotNull final Player player) {
        return this.chatters.get(player.getUniqueId());
    }

    @Override
    public @Nullable IChatter getChatter(@NotNull final UUID uuid) {
        return this.chatters.get(uuid);
    }

    @Override
    public boolean loadChatter(@NotNull final Player player) {
        if (this.hasChatter(player.getUniqueId())) {
            return false;
        }

        try {
            IChatter chatter = this.storage.load(player);
            this.chatters.put(chatter.getPlayer().getUniqueId(), chatter);

            IChats instance = Chats.getInstance();
            if (instance != null && instance.getConfigManager().isLogDebug()) {
                instance.getLogger().info("Loaded Chatter '" + chatter.getPlayer().getName() + "' with UUID '"
                        + chatter.getPlayer().getUniqueId() + "'.");
            }

            return true;
        } catch (InvalidStorageFileException | IOException ex) {
            IChats instance = Chats.getInstance();
            if (instance != null) {
                instance.getLogger().warning("Failed to load Chatter '" + player.getName() + "': "
                        + ex.getMessage());
            }

            return false;
        }
    }

    @Override
    public boolean unloadChatter(@NotNull final Player player) {
        return this.unloadChatter(player.getUniqueId());
    }

    @Override
    public boolean unloadChatter(@NotNull final UUID uuid) {
        Chatter chatter = (Chatter) this.chatters.remove(uuid);
        IChats instance = Chats.getInstance();

        if (chatter == null) {
            if (instance != null) {
                instance.getLogger().warning("Failed to unload Chatter with UUID '" + uuid
                        + "'. Chatter not found.");
            }

            return false;
        }

        this.storage.update(chatter);

        for (IChannel current : new HashSet<>(chatter.getChannels())) {
            chatter._removeChannel(current);
        }

        if (instance != null && instance.getConfigManager().isLogDebug()) {
            instance.getLogger().info("Unloaded Chatter '" + chatter.getPlayer().getName() + "' with UUID '"
                    + chatter.getPlayer().getUniqueId() + "'.");
        }

        return true;
    }
}
