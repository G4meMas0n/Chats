package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Placeholder;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

/**
 * Representation of the channel formatter, implements the {@link IChannelFormatter} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 15th, 2019
 * last change: November 13th, 2019
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

    protected ChannelFormatter(@NotNull final IChannel channel) {
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

        formatted = formatted.replace(Placeholder.MESSAGE.toString(), message);

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

        formatted = formatted.replace(Placeholder.MESSAGE.toString(), message);

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

        formatted = formatted.replace(Placeholder.SENDER.toString(), player.getDisplayName());
        formatted = formatted.replace(Placeholder.SENDER_PLAIN.toString(), player.getName());
        formatted = formatted.replace(Placeholder.SENDER_WORLD.toString(), player.getWorld().getName());
        formatted = formatted.replace(Placeholder.MESSAGE.toString(), message);

        if (chatService != null) {
            final World world = player.getWorld();
            final String group = chatService.getPrimaryGroup(player);

            formatted = formatted.replace(Placeholder.SENDER_GROUP.toString(), group);
            formatted = formatted.replace(Placeholder.SENDER_GROUP_PREFIX.toString(),
                    chatService.getGroupPrefix(world, group));
            formatted = formatted.replace(Placeholder.SENDER_GROUP_SUFFIX.toString(),
                    chatService.getGroupSuffix(world, group));
            formatted = formatted.replace(Placeholder.SENDER_PREFIX.toString(),
                    chatService.getPlayerPrefix(player));
            formatted = formatted.replace(Placeholder.SENDER_SUFFIX.toString(),
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

        format = format.replace(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replace(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replace(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

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
        format = format.replace(Placeholder.BROADCAST_PREFIX.toString(), "Broadcast");
        format = format.replace(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replace(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replace(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

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

        format = format.replace(Placeholder.CHANNEL_NAME.toString(), this.channel.getFullName());
        format = format.replace(Placeholder.CHANNEL_NICK.toString(), this.channel.getShortName());
        format = format.replace(Placeholder.CHANNEL_COLOR.toString(), this.channel.getChatColor().toString());

        return format;
    }
}
