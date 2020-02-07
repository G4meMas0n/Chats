package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.ConfigKey;
import de.g4memas0n.Chats.util.Placeholder;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.World;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Representation of the chat formatter, implements the {@link IChatFormatter} interface.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 5th, 2020
 * changed: January 17th, 2020
 */
public final class ChatFormatter implements IChatFormatter {

    private static final String FORMAT_INVALID = "Missing %s Placeholder";

    private String announceFormat;
    private String broadcastFormat;
    private String chatFormat;
    private ChatColor chatColor;

    private String conversationFormat;
    private ChatColor conversationColor;

    private Chat service;

    public ChatFormatter() {

    }

    @Override
    public @Nullable Chat getChatService() {
        return this.service;
    }

    @Override
    public boolean setChatService(@Nullable final Chat service) {
        if (service == this.service) {
            return false;
        }

        this.service = service;
        return true;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        if (this.announceFormat == null) {
            return ConfigKey.FORMAT_ANNOUNCE.getDefault();
        }

        return this.announceFormat;
    }

    @Override
    public boolean setAnnounceFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.announceFormat)) {
            return false;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.MESSAGE));
        }

        this.announceFormat = format;
        return true;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        if (this.broadcastFormat == null) {
            return ConfigKey.FORMAT_BROADCAST.getDefault();
        }

        return this.broadcastFormat;
    }

    @Override
    public boolean setBroadcastFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.broadcastFormat)) {
            return false;
        }

        if (!format.contains(Placeholder.BROADCAST_PREFIX.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.BROADCAST_PREFIX));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.MESSAGE));
        }

        this.broadcastFormat = format;
        return true;
    }

    @Override
    public @NotNull String getChatFormat() {
        if (this.chatFormat == null) {
            return ConfigKey.FORMAT_CHAT.getDefault();
        }

        return this.chatFormat;
    }

    @Override
    public boolean setChatFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.chatFormat)) {
            return false;
        }

        if (!format.contains(Placeholder.SENDER.toString()) || !format.contains(Placeholder.SENDER_PLAIN.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID,
                    Placeholder.SENDER + " or " + Placeholder.SENDER_PLAIN));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.MESSAGE));
        }

        this.chatFormat = format;
        return true;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        if (this.chatColor == null) {
            return ConfigKey.COLOR_CHANNEL.getDefault();
        }

        return this.chatColor;
    }

    @Override
    public boolean setChatColor(@NotNull final ChatColor color) {
        if (color == this.chatColor) {
            return false;
        }

        this.chatColor = color;
        return true;
    }

    @Override
    public @NotNull String getConversationFormat() {
        if (this.conversationFormat == null) {
            return ConfigKey.FORMAT_CONVERSATION.getDefault();
        }

        return this.conversationFormat;
    }

    @Override
    public boolean setConversationFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.conversationFormat)) {
            return false;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.MESSAGE));
        }

        if (!format.contains(Placeholder.CON_PARTNER.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.CON_PARTNER));
        }

        if (!format.contains(Placeholder.CON_SENDER.toString())
                || !format.contains(Placeholder.CON_ADDRESS.toString())) {
            throw new IllegalArgumentException(String.format(FORMAT_INVALID, Placeholder.CON_SENDER + " or "
                    + Placeholder.CON_ADDRESS));
        }

        this.conversationFormat = format;
        return true;
    }

    @Override
    public @NotNull ChatColor getConversationColor() {
        if (this.conversationColor == null) {
            return ConfigKey.COLOR_CONVERSATION.getDefault();
        }

        return this.conversationColor;
    }

    @Override
    public boolean setConversationColor(@NotNull final ChatColor color) {
        if (color == this.conversationColor) {
            return false;
        }

        this.conversationColor = color;
        return true;
    }

    @Override
    public @NotNull String formatAnnounce(@NotNull final IChannel channel,
                                          @NotNull final String format,
                                          @NotNull final String message) {
        String formatted = format.replace(Placeholder.CHANNEL_COLOR.toString(), channel.getChatColor().toString())
                .replace(Placeholder.CHANNEL_NAME.toString(), channel.getFullName())
                .replace(Placeholder.CHANNEL_NICK.toString(), channel.getShortName())
                .replace(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public @NotNull String formatBroadcast(@NotNull final IChannel channel,
                                           @NotNull final String format,
                                           @NotNull final String message) {
        String formatted = format.replace(Placeholder.BROADCAST_PREFIX.toString(), "Broadcast") //TODO: Add localized 'prefix_broadcast' message.
                .replace(Placeholder.CHANNEL_COLOR.toString(), channel.getChatColor().toString())
                .replace(Placeholder.CHANNEL_NAME.toString(), channel.getFullName())
                .replace(Placeholder.CHANNEL_NICK.toString(), channel.getShortName())
                .replace(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public @NotNull String formatChat(@NotNull final IChannel channel,
                                      @NotNull final String format,
                                      @NotNull final IChatter sender,
                                      @NotNull final String message) {
        String formatted = format.replace(Placeholder.CHANNEL_COLOR.toString(), channel.getChatColor().toString())
                .replace(Placeholder.CHANNEL_NAME.toString(), channel.getFullName())
                .replace(Placeholder.CHANNEL_NICK.toString(), channel.getShortName())
                .replace(Placeholder.SENDER.toString(), sender.getPlayer().getDisplayName())
                .replace(Placeholder.SENDER_PLAIN.toString(), sender.getPlayer().getName())
                .replace(Placeholder.SENDER_WORLD.toString(), sender.getPlayer().getWorld().getName());

        if (this.service != null) {
            final World world = sender.getPlayer().getWorld();
            final String group = service.getPrimaryGroup(sender.getPlayer());

            formatted = formatted.replace(Placeholder.SENDER_GROUP.toString(), group)
                    .replace(Placeholder.SENDER_GROUP_PREFIX.toString(), service.getGroupPrefix(world, group))
                    .replace(Placeholder.SENDER_GROUP_SUFFIX.toString(), service.getGroupSuffix(world, group))
                    .replace(Placeholder.SENDER_PREFIX.toString(), service.getPlayerPrefix(sender.getPlayer()))
                    .replace(Placeholder.SENDER_SUFFIX.toString(), service.getPlayerSuffix(sender.getPlayer()));
        }

        return Placeholder.stripPlaceholders(formatted.replace(Placeholder.MESSAGE.toString(), message));
    }

    @Override
    public @NotNull String formatConversation(@NotNull final String format,
                                              @NotNull final IChatter sender,
                                              @NotNull final IChatter partner,
                                              @NotNull final String message) {
        String formatted = format.replace(Placeholder.CHANNEL_COLOR.toString(), this.getConversationColor().toString())
                .replace(Placeholder.CON_SENDER.toString(), sender.getPlayer().getDisplayName())
                .replace(Placeholder.CON_PARTNER.toString(), partner.getPlayer().getDisplayName())
                .replace(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public @NotNull String formatAddress(@NotNull final String format,
                                         @NotNull final String address,
                                         @NotNull final IChatter partner,
                                         @NotNull final String message) {
        String formatted = format.replace(Placeholder.CHANNEL_COLOR.toString(), this.getConversationColor().toString())
                .replace(Placeholder.CON_ADDRESS.toString(), address)
                .replace(Placeholder.CON_PARTNER.toString(), partner.getPlayer().getDisplayName())
                .replace(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }
}
