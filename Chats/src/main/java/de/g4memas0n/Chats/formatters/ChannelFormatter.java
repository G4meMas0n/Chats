package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.chatters.IChatter;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

/**
 * Implements the IChannelFormatter interface, that handle the message formatting of channels.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 15th, 2019
 * last change: September 11th, 2019
 */
public final class ChannelFormatter implements IChannelFormatter {

    /**
     * The empty string that will be returned when the channel stored in this formatter do not support an feature.
     */
    private static final String EMPTY_STRING = "";

    private final IChannel channel;
    private String preAnnounceFormat;
    private String preBroadcastFormat;
    private String preChannelFormat;
    private boolean loggableAnnounce;
    private boolean loggableBroadcast;
    private boolean loggableChannel;

    public ChannelFormatter(@NotNull final IChannel channel) {
        this.channel = channel;
        this.update();
    }

    public void update() throws IllegalArgumentException {
        String newFormat = this.channel.getChannelFormat();

        if (newFormat.isEmpty()) {
            throw new IllegalArgumentException("Channel " + this.channel.getFullName() + " returned empty channel format.");
        }

        this.preChannelFormat = this.prepareChannelFormat(newFormat);

        newFormat = this.channel.getAnnounceFormat();
        this.preAnnounceFormat = newFormat.isEmpty() ? null : this.prepareAnnounceFormat(newFormat);

        newFormat = this.channel.getBroadcastFormat();
        this.preBroadcastFormat = newFormat.isEmpty() ? null : this.prepareBroadcastFormat(newFormat);
    }

    @Override
    public boolean isAnnounceLoggable() {
        return loggableAnnounce;
    }

    @Override
    public @NotNull String formatAnnounce(@NotNull final String message) {
        if (this.preAnnounceFormat == null) {
            return EMPTY_STRING;
        }

        String formatted = this.preAnnounceFormat;

        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public boolean isBroadcastLoggable() {
        return loggableBroadcast;
    }

    @Override
    public @NotNull String formatBroadcast(@NotNull final String message) {
        if (this.preBroadcastFormat == null) {
            return EMPTY_STRING;
        }

        String formatted = this.preBroadcastFormat;

        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public boolean isChatLoggable() {
        return loggableChannel;
    }

    @Override
    public @NotNull String formatChat(@NotNull final IChatter chatter, @NotNull final String message) {
        final Player player = chatter.getPlayer();
        final Chat chatService = Chats.getInstance() != null ? Chats.getInstance().getChatService() : null;
        String formatted = this.preChannelFormat;

        formatted = formatted.replaceFirst(Placeholder.SENDER.toString(), player.getDisplayName());
        formatted = formatted.replaceFirst(Placeholder.SENDER_PLAIN.toString(), player.getName());
        formatted = formatted.replaceFirst(Placeholder.SENDER_WORLD.toString(), player.getWorld().getName());
        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        if (chatService != null) {
            final World world = player.getWorld();
            final String group = chatService.getPrimaryGroup(player);

            formatted = formatted.replaceFirst(Placeholder.SENDER_GROUP.toString(), group);
            formatted = formatted.replaceFirst(Placeholder.SENDER_GROUP_PREFIX.toString(),
                    chatService.getGroupPrefix(world, group));
            formatted = formatted.replaceFirst(Placeholder.SENDER_GROUP_SUFFIX.toString(),
                    chatService.getGroupSuffix(world, group));
            formatted = formatted.replaceFirst(Placeholder.SENDER_PREFIX.toString(),
                    chatService.getPlayerPrefix(player));
            formatted = formatted.replaceFirst(Placeholder.SENDER_SUFFIX.toString(),
                    chatService.getPlayerSuffix(player));
        }

        return Placeholder.stripPlaceholders(formatted);
    }

    /**
     * Prepares the given announce format with the information of the channel stored in this formatter.
     * @param format the announce format that should be prepared.
     * @return the prepared announce format.
     */
    private @NotNull String prepareAnnounceFormat(@NotNull String format) {
        loggableAnnounce = format.contains(Placeholder.CHANNEL_NAME.toString())
                || format.contains(Placeholder.CHANNEL_NICK.toString());

        format = format.replaceFirst(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replaceFirst(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

        return format;
    }

    /**
     * Prepares the given broadcast format with the information of the channel stored in this formatter.
     * @param format the broadcast format that should be prepared.
     * @return the prepared broadcast format.
     */
    private @NotNull String prepareBroadcastFormat(@NotNull String format) {
        loggableBroadcast = format.contains(Placeholder.CHANNEL_NAME.toString())
                || format.contains(Placeholder.CHANNEL_NICK.toString());

        //TODO: Use Resource Bundle for getting correct broadcast prefix
        format = format.replaceFirst(Placeholder.BROADCAST_PREFIX.toString(), "Broadcast");
        format = format.replaceFirst(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replaceFirst(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

        return format;
    }

    /**
     * Prepares the given channel format with the information of the channel stored in this formatter.
     * @param format the channel format that should be prepared.
     * @return the prepared channel format.
     */
    private @NotNull String prepareChannelFormat(@NotNull String format) {
        loggableChannel = format.contains(Placeholder.CHANNEL_NAME.toString())
                || format.contains(Placeholder.CHANNEL_NICK.toString());

        format = format.replaceFirst(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replaceFirst(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

        return format;
    }
}
