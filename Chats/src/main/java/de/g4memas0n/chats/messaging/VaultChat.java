package de.g4memas0n.chats.messaging;

import net.milkbowl.vault.chat.Chat;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Implementation of the Chat Service from supported plugin Vault.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class VaultChat implements IChat {

    private final Chat service;

    public VaultChat(@NotNull final Chat service) {
        this.service = service;
    }

    @Override
    public @Nullable String getGroup(@NotNull final Player player) {
        return this.service.getPrimaryGroup(player);
    }

    @Override
    public @NotNull String getGroupPrefix(@NotNull final Player player) {
        final String group = this.service.getPrimaryGroup(player);

        if (group == null) {
            return "";
        }

        return this.getGroupPrefix(player.getWorld(), group);
    }

    @Override
    public @NotNull String getGroupPrefix(@NotNull final World world, @NotNull final String group) {
        final String prefix = this.service.getGroupPrefix(world, group);

        if (prefix == null) {
            return "";
        }

        return prefix;
    }

    @Override
    public @NotNull String getGroupSuffix(@NotNull final Player player) {
        final String group = this.service.getPrimaryGroup(player);

        if (group == null) {
            return "";
        }

        return this.getGroupSuffix(player.getWorld(), group);
    }

    @Override
    public @NotNull String getGroupSuffix(@NotNull final World world, @NotNull final String group) {
        final String suffix = this.service.getGroupSuffix(world, group);

        if (suffix == null) {
            return "";
        }

        return suffix;
    }

    @Override
    public @NotNull String getPlayerPrefix(@NotNull final Player player) {
        final String prefix = this.service.getPlayerPrefix(player);

        if (prefix == null) {
            return "";
        }

        return prefix;
    }

    @Override
    public @NotNull String getPlayerSuffix(@NotNull final Player player) {
        final String suffix = this.service.getPlayerSuffix(player);

        if (suffix == null) {
            return "";
        }

        return suffix;
    }

    @Override
    public @NotNull String getPrefix(@NotNull final Player player) {
        final String prefix = this.service.getPlayerPrefix(player);

        if (prefix == null) {
            return this.getGroupPrefix(player);
        }

        return prefix;
    }

    @Override
    public @NotNull String getSuffix(@NotNull final Player player) {
        final String suffix = this.service.getPlayerSuffix(player);

        if (suffix == null) {
            return this.getGroupSuffix(player);
        }

        return suffix;
    }
}
