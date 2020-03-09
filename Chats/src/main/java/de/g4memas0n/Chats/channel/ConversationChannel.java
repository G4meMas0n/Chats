package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.event.chatter.ChatterChatConversationEvent;
import de.g4memas0n.Chats.messaging.Messages;
import de.g4memas0n.Chats.messaging.Placeholder;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.chatter.IChatter;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Representation of a conversation channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: March 4th, 2020
 */
public class ConversationChannel extends StandardChannel {

    private static final String CONVERSATION_NAME = "Conversation";

    private final IChannelManager manager;

    public ConversationChannel(@NotNull final IChannelManager manager,
                               @NotNull final IFormatter formatter,
                               @NotNull final IChatter first,
                               @NotNull final IChatter second) throws IllegalArgumentException {
        super(formatter, IChannel.buildConversationName(first, second));

        this.manager = manager;
    }

    // Channel Properties Methods:
    @Override
    public @NotNull String getShortName() {
        return CONVERSATION_NAME;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        return false;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        return this.getFormatter().getConversationColor();
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) {
        return false;
    }

    @Override
    public boolean hasPassword() {
        return false;
    }

    @Override
    public @Nullable String getPassword() {
        return null;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        return false;
    }

    @Override
    public boolean isCrossWorld() {
        return true;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        return false;
    }

    @Override
    public boolean hasDistance() {
        return false;
    }

    @Override
    public int getDistance() {
        return -1;
    }

    @Override
    public boolean setDistance(final int distance) {
        return false;
    }

    @Override
    public boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final ConversationChannel channel = (ConversationChannel) object;
        return this.getFullName().equals(channel.getFullName());
    }

    @Override
    public int hashCode() {
        final int prime = 67;
        int result = 2;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getType() {
        return ChannelType.CONVERSATION;
    }

    @Override
    public boolean isConversation() {
        return true;
    }

    // Channel Collection Methods:
    public @Nullable IChatter getPartner(@NotNull final IChatter sender) {
        for (final IChatter current : this.getChatters()) {
            if (current.equals(sender)) {
                continue;
            }

            if (this.getFullName().contains(current.getPlayer().getUniqueId().toString())) {
                return current;
            }
        }

        return null;
    }

    @Override
    public boolean addChatter(@NotNull final IChatter chatter) {
        if (this.hasChatter(chatter) || this.getChatters().size() >= 2) {
            return false;
        }

        if (this.getFullName().contains(chatter.getPlayer().getUniqueId().toString())) {
            return super.addChatter(chatter);
        }

        return false;
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        if (!this.hasChatter(chatter)) {
            return false;
        }

        if (super.removeChatter(chatter)) {
            if (this.getChatters().size() < 2) {
                this.manager.removeChannel(this);
            }

            return true;
        }

        return false;
    }

    // Channel Formatter and Performer Methods:
    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean isCustomFormat() {
        return false;
    }

    @Override
    public boolean setCustomFormat(final boolean enabled) {
        return false;
    }

    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.hasChatter(sender)) {
            return;
        }

        final IChatter partner = this.getPartner(sender);

        if (partner == null) {
            sender.getPlayer().sendMessage(Messages.tlErr("partnerNotFound"));
            return;
        }

        final ChatterChatConversationEvent event = new ChatterChatConversationEvent(sender, partner,
                this.getFormatter().getConversationFormat(), message);

        Bukkit.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        //Log.conversation(sender, partner, event.getMessage());

        if (event.getFormat().contains(Placeholder.CON_ADDRESS.toString())) {
            final String to = this.getFormatter().formatAddress(event.getFormat(), Messages.tl("addressTo"),
                    partner, event.getMessage());
            final String from = this.getFormatter().formatAddress(event.getFormat(), Messages.tl("From"),
                    sender, event.getMessage());

            sender.getPlayer().sendMessage(to);
            sender.setLastPartners(partner);

            partner.getPlayer().sendMessage(from);
            partner.setLastPartners(sender);

            return;
        }

        final String output = this.getFormatter().formatConversation(event.getFormat(), sender, partner,
                event.getMessage());

        sender.getPlayer().sendMessage(output);
        sender.setLastPartners(partner);

        partner.getPlayer().sendMessage(output);
        partner.setLastPartners(sender);
    }
}
