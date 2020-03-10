package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.channel.IChannel;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
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
 * changed: March 9th, 2020
 */
public final class ChatterManager implements IChatterManager {

    private static final String DIRECTORY_NAME = "chatters";

    private final Map<UUID, IChatter> chatters;
    private final IChats instance;
    private final File directory;

    public ChatterManager(@NotNull final IChats instance) {
        this.chatters = new HashMap<>();
        this.instance = instance;
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);
    }

    @Override
    public @NotNull Set<IChatter> getChatters() {
        return new HashSet<>(this.chatters.values());
    }

    @Override
    public @Nullable IChatter getChatter(@NotNull final UUID uniqueId) {
        return this.chatters.get(uniqueId);
    }

    @Override
    public @NotNull IChatter getChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        return this.loadChatter(player);
    }

    @Override
    public @NotNull IChatter loadChatter(@NotNull final Player player) {
        if (this.chatters.containsKey(player.getUniqueId())) {
            return this.chatters.get(player.getUniqueId());
        }

        final IChatter chatter = new StandardChatter(player, this.instance.getChannelManager(), this.getFile(player));

        this.chatters.put(player.getUniqueId(), chatter);

        return chatter;
    }

    @Override
    public boolean unloadChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.containsKey(chatter.getPlayer().getUniqueId())) {
            return false;
        }

        for (final IChannel current : chatter.getChannels()) {
            current.setMember(chatter, false);
        }

        this.chatters.remove(chatter.getPlayer().getUniqueId());
        return true;
    }

    @Override
    public boolean hasChatter(@NotNull final UUID uniqueId) {
        return this.chatters.containsKey(uniqueId);
    }

    @Override
    public void reload() {
        for (final IChatter current : this.chatters.values()) {
            current.load();
        }
    }

    private @NotNull File getFile(@NotNull final Player player) {
        return new File(this.directory, player.getUniqueId() + ".yml");
    }
}
