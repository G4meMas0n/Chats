package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import de.g4memas0n.chats.util.type.InfoType;
import de.g4memas0n.chats.util.type.ModifyType;
import de.g4memas0n.chats.util.type.StorageType;
import org.bukkit.command.BlockCommandSender;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.jetbrains.annotations.NotNull;

/**
 * CommandSource representation for the {@link ConsoleCommandSender} and {@link BlockCommandSender}, implements
 * {@link ICommandSource}.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: April 4th, 2020
 * changed: June 22th, 2020
 */
public final class CommandSource implements ICommandSource {

    private final CommandSender sender;
    private final boolean console;

    public CommandSource(@NotNull final CommandSender sender) {
        this.sender = sender;
        this.console = sender instanceof ConsoleCommandSender;
    }

    @Override
    public void sendMessage(@NotNull final String message) {
        if (message.isEmpty()) {
            return;
        }

        this.sender.sendMessage(message);
    }

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
        if (chatter.hasPermission(Permission.BAN.formChildren("exempt"))) {
            return false;
        }

        return this.console;
    }

    @Override
    public boolean canCreate(@NotNull final ChannelType type) {
        if (type == ChannelType.CONVERSATION) {
            return false;
        }

        return this.console;
    }

    @Override
    public boolean canDelete(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.console;
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
        if (chatter.hasPermission(Permission.KICK.formChildren("exempt"))) {
            return false;
        }

        return this.console;
    }

    @Override
    public boolean canLeave(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canList(@NotNull final ChannelType type) {
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

        return this.console;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel) {
        if (channel.isConversation()) {
            return false;
        }

        return this.console;
    }

    @Override
    public boolean canModify(@NotNull final IChannel channel, @NotNull final ModifyType type) {
        if (channel.isConversation()) {
            return false;
        }

        return this.console;
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
        if (chatter.hasPermission(Permission.MUTE.formChildren("exempt"))) {
            return false;
        }

        return this.console;
    }

    @Override
    public boolean canReload(@NotNull final StorageType type) {
        return this.console;
    }

    @Override
    public boolean canSave(@NotNull final StorageType type) {
        return this.console;
    }

    @Override
    public boolean canSee(@NotNull final IChatter chatter) {
        return this.console;
    }

    @Override
    public boolean canSpeak(@NotNull final IChannel channel) {
        return false;
    }

    @Override
    public boolean canView(@NotNull final IChannel channel, @NotNull final InfoType type) {
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
