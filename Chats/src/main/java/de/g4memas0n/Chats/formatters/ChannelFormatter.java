package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.util.ANSIColor;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class ChannelFormatter implements IFormatter {
    private final Chat chatService;
    private final String channelFormat;
    private final ChatColor chatColor;

    public ChannelFormatter(@Nullable final Chat chatService,
                            @NotNull final IChannel channel,
                            @NotNull final IChatter sender,
                            @NotNull final String message,
                            @NotNull final String format) {
        this.chatService = chatService;
        this.channelFormat = this.formatMessage(channel, sender, message, format);
        this.chatColor = channel.getChatColor();
    }

    @NotNull
    private String formatMessage(@NotNull final IChannel channel,
                                 @NotNull final IChatter sender,
                                 @NotNull final String message,
                                 @NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL.toString(), channel.getFullName());
        format = format.replaceAll(Placeholder.CHANNEL_NICK.toString(), channel.getShortName());

        format = format.replaceAll(Placeholder.MESSAGE.toString(), message);

        Player senderPlayer = sender.getPlayer();
        World senderWorld = senderPlayer.getWorld();

        format = format.replaceAll(Placeholder.SENDER.toString(), senderPlayer.getDisplayName());
        format = format.replaceAll(Placeholder.SENDER_PLAIN.toString(), senderPlayer.getName());
        format = format.replaceAll(Placeholder.SENDER_WORLD.toString(), senderWorld.getName());

        if (this.chatService != null) {
            format = format.replaceAll(Placeholder.SENDER_PREFIX.toString(), this.chatService.getPlayerPrefix(senderPlayer));
            format = format.replaceAll(Placeholder.SENDER_SUFFIX.toString(), this.chatService.getPlayerSuffix(senderPlayer));

            String senderGroup = this.chatService.getPrimaryGroup(senderPlayer);
            format = format.replaceAll(Placeholder.SENDER_GROUP.toString(), senderGroup);
            format = format.replaceAll(Placeholder.SENDER_GROUP_PREFIX.toString(), this.chatService.getGroupPrefix(senderWorld, senderGroup));
            format = format.replaceAll(Placeholder.SENDER_GROUP_SUFFIX.toString(), this.chatService.getGroupSuffix(senderWorld, senderGroup));
        } else {
            format = format.replaceAll(Placeholder.SENDER_PREFIX.toString(), "");
            format = format.replaceAll(Placeholder.SENDER_SUFFIX.toString(), "");
            format = format.replaceAll(Placeholder.SENDER_GROUP.toString(), "");
            format = format.replaceAll(Placeholder.SENDER_GROUP_PREFIX.toString(), "");
            format = format.replaceAll(Placeholder.SENDER_GROUP_SUFFIX.toString(), "");
        }

        return format;
    }

    @Override
    @NotNull
    public String format() {
        return this.channelFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.chatColor.toString());
    }

    @Override
    @NotNull
    public String formatLog(final boolean colored) {
        if (colored) {
            return this.channelFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                    ANSIColor.getByBukkitColor(this.chatColor).toString()) + ANSIColor.RESET.toString();
        } else {
            return this.channelFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), "");
        }
    }
}