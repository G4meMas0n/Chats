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
import java.util.UUID;

/**
 * Representation of a conversation channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: March 9th, 2020
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
    public @Nullable String getPassword() {
        return null;
    }

    @Override
    public boolean hasPassword() {
        return false;
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
    public int getDistance() {
        return -1;
    }

    @Override
    public boolean hasDistance() {
        return false;
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
    public final @NotNull ChannelType getType() {
        return ChannelType.CONVERSATION;
    }

    @Override
    public final boolean isConversation() {
        return true;
    }

    @Override
    public final boolean isPersist() {
        return false;
    }

    // Channel Collection Methods:
    public final @Nullable IChatter getPartner(@NotNull final IChatter sender) {
        for (final IChatter current : this.getMembers()) {
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
    public boolean setMember(@NotNull final IChatter chatter, final boolean member) {
        if (member && this.getFullName().contains(chatter.getPlayer().getUniqueId().toString())) {
            return super.setMember(chatter, true);
        } else {
            return super.setMember(chatter, false);
        }
    }

    @Override
    public boolean addMember(@NotNull final IChatter chatter) {
        if (this.isMember(chatter) || this.getMembers().size() >= 2) {
            return false;
        }

        if (this.getFullName().contains(chatter.getPlayer().getUniqueId().toString())) {
            return super.addMember(chatter);
        }

        return false;
    }

    @Override
    public boolean removeMember(@NotNull final IChatter chatter) {
        if (!this.isMember(chatter)) {
            return false;
        }

        if (super.removeMember(chatter)) {
            if (this.getMembers().size() < 2) {
                this.manager.removeChannel(this);
            }

            return true;
        }

        return false;
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        return false;
    }

    @Override
    public boolean banMember(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean unBanMember(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean isBanned(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean kickMember(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        return false;
    }

    @Override
    public boolean muteMember(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean unMuteMember(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean isMuted(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator) {
        return false;
    }

    @Override
    public boolean addModerator(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean removeModerator(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean isModerator(@NotNull final IChatter chatter) {
        return false;
    }

    @Override
    public boolean hasOwner() {
        return false;
    }

    public @Nullable UUID getOwner() {
        return null;
    }

    @Override
    public boolean setOwner(@Nullable final UUID uniqueId) {
        return false;
    }

    @Override
    public boolean isOwner(@NotNull final UUID uniqueId) {
        return false;
    }

    // Formatting Methods:
    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.getFormatter().getConversationFormat();
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
    public boolean setCustomFormat(final boolean customFormat) {
        return false;
    }

    // Performing Methods:
    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.isMember(sender)) {
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
            final String to = this.getFormatter().formatAddress(event.getFormat(), Messages.tl("to"),
                    partner, event.getMessage());
            final String from = this.getFormatter().formatAddress(event.getFormat(), Messages.tl("from"),
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
