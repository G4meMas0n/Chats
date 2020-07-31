package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.channel.IChannel.Information;
import de.g4memas0n.chats.channel.IChannel.Modification;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.storage.IStorageHolder;
import org.bukkit.command.BlockCommandSender;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.List;

/**
 * Implementation of a command source for the Bukkit/Spigot {@link ConsoleCommandSender} and {@link BlockCommandSender}.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class BasicCommandSource implements ICommandSource {

    private final CommandSender sender;

    public BasicCommandSource(@NotNull final CommandSender sender) {
        this.sender = sender;
    }

    // IMessageRecipient Implementation:
    @Override
    public void sendMessage(@NotNull final String message) {
        if (message.isEmpty()) {
            return;
        }

        this.sender.sendMessage(message);
    }

    @Override
    public void sendMessage(@NotNull final List<String> messages) {
        for (final String message : messages) {
            if (message.isEmpty()) {
                continue;
            }

            this.sender.sendMessage(message);
        }
    }

    // IPermissible Implementation:
    @Override
    public boolean hasPermission(@NotNull final String node) {
        return this.sender.hasPermission(node);
    }

    @Override
    public boolean canBan(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be banned.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to be excepted from channel bans.
        if (chatter.hasPermission(Permission.BAN.getChildren("exempt"))) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canCreate(@NotNull final IChannel.Type type) {
        if (type == IChannel.Type.CONVERSATION) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canFocus(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canIgnore(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean canJoin(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canKick(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be banned.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to be excepted from channel bans.
        if (chatter.hasPermission(Permission.KICK.getChildren("exempt"))) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canLeave(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canList(@NotNull final IChannel.Type type) {
        return true;
    }

    @Override
    public boolean canList(@NotNull final IChannel channel) {
        return true;
    }

    @Override
    public boolean canMessage(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean canModerate(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final Modification modification) {
        if (channel.isConversation()) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canMute(@NotNull final IChatter chatter, @NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        // Check if chatter is the owner of the channel, because the owner of a channel can not be banned.
        if (channel.isOwner(chatter.getUniqueId())) {
            return false;
        }

        // Check if chatter has permission to be excepted from channel bans.
        if (chatter.hasPermission(Permission.MUTE.getChildren("exempt"))) {
            return false;
        }

        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canReload(@NotNull final IStorageHolder.Type type) {
        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canSave(@NotNull final IStorageHolder.Type type) {
        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canSee(@NotNull final IChatter chatter) {
        return this.sender instanceof ConsoleCommandSender;
    }

    @Override
    public boolean canSpeak(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canView(@NotNull final IChannel channel, @NotNull final Information information) {
        return !channel.isConversation();
    }

    @Override
    public boolean canViewInfo(@NotNull final IChannel channel) {
        return !channel.isConversation();
    }

    @Override
    public boolean canViewWho(@NotNull final IChannel channel) {
        return !channel.isConversation();
    }
}
