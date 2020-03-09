package de.g4memas0n.Chats.messaging;

import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.World;
import org.jetbrains.annotations.NotNull;

/**
 * Representation of the formatter, implements the {@link IFormatter} interface.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 5th, 2020
 * changed: February 15th, 2020
 */
public final class Formatter implements IFormatter {

    private final IChats instance;

    public Formatter(@NotNull final IChats instance) {
        this.instance = instance;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        return this.instance.getSettings().getAnnounceFormat();
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        return this.instance.getSettings().getBroadcastFormat();
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.instance.getSettings().getChatFormat();
    }

    @Override
    public @NotNull String getConversationFormat() {
        return this.instance.getSettings().getConversationFormat();
    }

    @Override
    public @NotNull ChatColor getChannelColor() {
        return this.instance.getSettings().getChannelColor();
    }

    @Override
    public @NotNull ChatColor getConversationColor() {
        return this.instance.getSettings().getConversationColor();
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
        String formatted = format.replace(Placeholder.BROADCAST_PREFIX.toString(), Messages.tl("broadcastPrefix"))
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

        final Chat service = this.instance.getChatService();

        if (service != null) {
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
