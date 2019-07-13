package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.util.ANSIColor;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;

public final class BroadcastFormatter implements IFormatter {
    private final String broadcastFormat;
    private final ChatColor chatColor;

    public BroadcastFormatter(@NotNull final IChannel channel,
                              @NotNull final String broadcastPrefix,
                              @NotNull final String message,
                              @NotNull final String format) {
        this.broadcastFormat = this.formatMessage(channel, broadcastPrefix, message, format);
        this.chatColor = channel.getChatColor();
    }

    @NotNull
    private String formatMessage(@NotNull final IChannel channel,
                                 @NotNull final String broadcastPrefix,
                                 @NotNull final String message,
                                 @NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL.toString(), channel.getFullName());
        format = format.replaceAll(Placeholder.CHANNEL_NICK.toString(), channel.getShortName());
        format = format.replaceAll(Placeholder.BROADCAST_PREFIX.toString(), broadcastPrefix);
        format = format.replaceAll(Placeholder.MESSAGE.toString(), message);

        return format;
    }

    @Override
    @NotNull
    public String format() {
        return this.broadcastFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.chatColor.toString());
    }

    @Override
    @NotNull
    public String formatLog(final boolean colored) {
        if (colored) {
            return this.broadcastFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                    ANSIColor.getByBukkitColor(this.chatColor).toString()) + ANSIColor.RESET.toString();
        } else {
            return this.broadcastFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), "");
        }
    }
}
