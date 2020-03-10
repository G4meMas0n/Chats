package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.util.type.ModifyType;
import de.g4memas0n.Chats.util.type.ReloadType;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

/**
 * Permissible representation for console and command block sender, implements {@link IPermissible}.
 *
 * @author G4meMason
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 8th, 2020
 * changed: March 9th, 2020
 */
public class SenderPermissible implements IPermissible {

    private final boolean console;

    public SenderPermissible(@NotNull final CommandSender sender) {
        this.console = sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canBroadcast(@NotNull final IChannel channel) {
        return true;
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
    public boolean canList(@NotNull final ChannelType type) {
        return false;
    }

    @Override
    public boolean canMessage(@NotNull final Player player) {
        return false;
    }

    @Override
    public boolean canModerate(@NotNull IChannel channel) {
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
    public boolean canViewInfo(@NotNull final IChannel channel) {
        return this.console;
    }

    @Override
    public boolean canViewWho(@NotNull final IChannel channel) {
        return this.console;
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
