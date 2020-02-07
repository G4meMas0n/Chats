package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.IChannelManager;
import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.channel.type.ModifyType;
import de.g4memas0n.Chats.util.ReloadType;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
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
 * changed: February 2nd, 2020
 */
public final class ChatterManager implements IChatterManager {

    private static final String DIRECTORY_NAME = "chatters";

    private final Map<UUID, IChatter> chatters;
    private final File directory;

    private final IChannelManager manager;

    public ChatterManager(@NotNull final IChannelManager manager,
                          @NotNull final File parent) throws IllegalArgumentException {
        if (!parent.isDirectory()) {
            throw new IllegalArgumentException("Parent File must be a directory");
        }

        this.chatters = new HashMap<>();
        this.directory = new File(parent, DIRECTORY_NAME);

        this.manager = manager;
    }

    @Override
    public @NotNull IPermissible getPermissible(@NotNull final CommandSender sender) {
        if (sender instanceof Player) {
            return this.getChatter((Player) sender);
        }

        return new SenderPermissible(sender);
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

        final IChatter chatter = new StandardChatter(player, this.manager, this.getFile(player));

        this.chatters.put(player.getUniqueId(), chatter);

        return chatter;
    }

    @Override
    public boolean unloadChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.containsKey(chatter.getPlayer().getUniqueId())) {
            return false;
        }

        for (final IChannel current : chatter.getChannels()) {
            current.removeChatter(chatter);
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
            current.reload();
        }
    }

    private @NotNull File getFile(@NotNull final Player player) {
        return new File(this.directory, player.getUniqueId() + ".yml");
    }

    private static class SenderPermissible implements IPermissible {

        private final boolean console;

        private SenderPermissible(@NotNull final CommandSender sender) {
            this.console = sender instanceof ConsoleCommandSender;
        }

        @Override
        public boolean canCreate(@NotNull final ChannelType type) {
            return this.console;
        }

        @Override
        public boolean canDelete(@NotNull final IChannel channel) {
            return this.console;
        }

        @Override
        public boolean canFocus(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean canIgnore(@NotNull final Player player) {
            return false;
        }

        @Override
        public boolean canJoin(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean canLeave(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean canMessage(@NotNull final Player player) {
            return false;
        }

        public boolean canModify(@NotNull final IChannel channel) {
            return this.console;
        }

        @Override
        public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
            return this.console;
        }

        @Override
        public boolean canReload(@NotNull final ReloadType type) {
            return this.console;
        }

        @Override
        public boolean canSpeak(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean forcedFocus(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean forcedJoin(@NotNull final IChannel channel) {
            return false;
        }

        @Override
        public boolean forcedLeave(@NotNull final IChannel channel) {
            return false;
        }
    }
}
