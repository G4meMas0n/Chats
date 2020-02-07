package de.g4memas0n.Chats.chat;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.event.channel.ChannelAnnounceEvent;
import de.g4memas0n.Chats.event.channel.ChannelBroadcastEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChatChannelEvent;
import de.g4memas0n.Chats.event.chatter.ChatterChatConversationEvent;
import de.g4memas0n.Chats.util.Placeholder;
import org.bukkit.Bukkit;
import org.jetbrains.annotations.NotNull;
import java.util.logging.Logger;

/**
 * Representations of the chat performer, implements the {@link IChatPerformer} interface.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 5th, 2020
 * changed: January 17th, 2020
 */
public final class ChatPerformer implements IChatPerformer {

    private static final String LOG_CHANNEL = "[%s] %s";
    private static final String LOG_CONVERSATION = "[%s -> %s] %s";

    private final IChatFormatter formatter;
    private Logger chatLogger;

    public ChatPerformer(@NotNull final IChatFormatter formatter) {
        this.formatter = formatter;
    }

    @Override
    public @NotNull Logger getChatLogger() {
        return this.chatLogger;
    }

    @Override
    public boolean setChatLogger(final @NotNull Logger logger) {
        if (logger.equals(this.chatLogger)) {
            return false;
        }

        this.chatLogger = logger;
        return true;
    }

    @Override
    public void performAnnounce(@NotNull final IChannel channel,
                                @NotNull final String message) {
        final ChannelAnnounceEvent event = new ChannelAnnounceEvent(channel, channel.getAnnounceFormat(), message);

        Bukkit.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = channel.getFormatter().formatAnnounce(channel, event.getFormat(), event.getMessage());

        if (this.chatLogger != null) {
            this.chatLogger.info(String.format(LOG_CHANNEL, channel.getFullName(), output));
        }

        channel.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performBroadcast(@NotNull final IChannel channel,
                                 @NotNull final String message) {
        final ChannelBroadcastEvent event = new ChannelBroadcastEvent(channel, channel.getBroadcastFormat(), message);

        Bukkit.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = channel.getFormatter().formatBroadcast(channel, event.getFormat(), event.getMessage());

        if (this.chatLogger != null) {
            this.chatLogger.info(String.format(LOG_CHANNEL, channel.getFullName(), output));
        }

        channel.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performChat(@NotNull final IChannel channel,
                            @NotNull final IChatter sender,
                            @NotNull final String message) {
        if (channel.isConversation()) {
            for (IChatter partner : channel.getChatters()) {
                if (!partner.equals(sender)) {
                    this.performConversion(sender, partner, message);
                    return;
                }
            }

            return;
        }

        final ChatterChatChannelEvent event = new ChatterChatChannelEvent(sender, channel, channel.getChatFormat(),
                message, false);

        Bukkit.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String output = channel.getFormatter().formatChat(channel, event.getFormat(), sender, event.getMessage());

        if (this.chatLogger != null) {
            this.chatLogger.info(String.format(LOG_CHANNEL, channel.getFullName(), output));
        }

        if (channel.hasDistance()) {
            final int distance = channel.getDistance();

            channel.getChatters().stream()
                    .filter(chatter -> chatter.isInRange(sender, distance))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        if (!channel.isCrossWorld()) {
            channel.getChatters().stream()
                    .filter(chatter -> chatter.isInWorld(sender))
                    .forEach(chatter -> chatter.getPlayer().sendMessage(output));

            return;
        }

        channel.getChatters().forEach(chatter -> chatter.getPlayer().sendMessage(output));
    }

    @Override
    public void performConversion(@NotNull final IChatter sender,
                                  @NotNull final IChatter partner,
                                  @NotNull final String message) {
        if (sender.equals(partner)) {
            return;
        }

        final ChatterChatConversationEvent event = new ChatterChatConversationEvent(sender, partner,
                this.formatter.getConversationFormat(), message);

        Bukkit.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        if (this.chatLogger != null) {
            this.chatLogger.info(String.format(LOG_CONVERSATION, sender.getPlayer().getName(), partner.getPlayer().getName(), event.getMessage()));
        }

        if (event.getFormat().contains(Placeholder.CON_ADDRESS.toString())) {
            final String to = this.formatter.formatAddress(event.getFormat(), "To", partner, event.getMessage());
            final String from = this.formatter.formatAddress(event.getFormat(), "From", sender, event.getMessage());

            sender.getPlayer().sendMessage(to);
            sender.setLastPartners(partner);

            partner.getPlayer().sendMessage(from);
            partner.setLastPartners(sender);

            return;
        }

        final String output = this.formatter.formatConversation(event.getFormat(), sender, partner, event.getMessage());

        sender.getPlayer().sendMessage(output);
        sender.setLastPartners(partner);

        partner.getPlayer().sendMessage(output);
        partner.setLastPartners(sender);
    }
}
