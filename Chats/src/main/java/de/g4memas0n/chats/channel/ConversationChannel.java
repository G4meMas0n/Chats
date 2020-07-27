package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.event.chatter.ChatterChatConversationEvent;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.text.MessageFormat;
import java.util.UUID;

/**
 * Implementation of a conversation channel that handles conversation actions between chatters (players).
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ConversationChannel extends StandardChannel {

    public ConversationChannel(@NotNull final Chats instance,
                               @NotNull final IChatter first,
                               @NotNull final IChatter second) throws IllegalArgumentException {
        super(instance, buildName(first, second));

        super.setShortName(buildShort(first, second));
        super.setColor(ChatColor.LIGHT_PURPLE);
        super.setVerbose(false);

        this.addMember(first);
        this.addMember(second);
    }

    // Channel Properties Methods:
    @Override
    public @NotNull String getColoredName() {
        return this.getColor() + this.getShortName();
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        return false;
    }

    @Override
    public boolean setColor(@Nullable final ChatColor color) {
        return false;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        return false;
    }

    @Override
    public boolean setDistance(final int distance) {
        return false;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        return false;
    }

    @Override
    public boolean setVerbose(final boolean verbose) {
        return false;
    }

    @Override
    public boolean setCustomFormat(final boolean customFormat) {
        return false;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        return this.instance.getFormatter().getAnnounceFormat();
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    public @NotNull String getBroadcastFormat() {
        return this.instance.getFormatter().getBroadcastFormat();
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.instance.getFormatter().getConversationFormat();
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) {
        return false;
    }

    // Channel Type Methods:
    @Override
    public final @NotNull ChannelType getType() {
        return ChannelType.CONVERSATION;
    }

    // Channel Collection Methods:
    protected final @Nullable IChatter getPartner(@NotNull final IChatter sender) {
        for (final IChatter current : this.getMembers()) {
            if (current.equals(sender)) {
                continue;
            }

            if (this.getFullName().contains(current.getUniqueId().toString())) {
                return current;
            }
        }

        return null;
    }

    @Override
    public synchronized boolean setMember(@NotNull final IChatter chatter, final boolean member) {
        if (!this.getFullName().contains(chatter.getUniqueId().toString())) {
            return false;
        }

        return super.setMember(chatter, member);
    }

    @Override
    public synchronized boolean addMember(@NotNull final IChatter chatter) {
        if (!this.getFullName().contains(chatter.getUniqueId().toString())) {
            return false;
        }

        return super.addMember(chatter, true);
    }

    @Override
    public synchronized boolean addMember(@NotNull final IChatter chatter, final boolean silent) {
        return this.addMember(chatter);
    }

    @Override
    public synchronized boolean removeMember(@NotNull final IChatter chatter) {
        if (!this.getFullName().contains(chatter.getUniqueId().toString())) {
            return false;
        }

        if (super.removeMember(chatter, true)) {
            this.instance.runSyncTask(() -> this.instance.getChannelManager().removeChannel(this));

            return true;
        }

        return false;
    }

    @Override
    public synchronized boolean removeMember(@NotNull final IChatter chatter, final boolean silent) {
        return this.removeMember(chatter);
    }

    @Override
    public boolean banMember(@NotNull final IChatter member) {
        return false;
    }

    @Override
    public boolean kickMember(@NotNull final IChatter member) {
        return false;
    }

    @Override
    public boolean muteMember(@NotNull final IChatter member) {
        return false;
    }

    @Override
    public boolean pardonMember(@NotNull final IOfflineChatter chatter) {
        return false;
    }

    @Override
    public boolean unmuteMember(@NotNull final IOfflineChatter member) {
        return false;
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        return false;
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        return false;
    }

    @Override
    public boolean setOwner(@Nullable final UUID uniqueId) {
        return false;
    }

    // Performing Methods:
    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message) {
        if (!this.isMember(sender) || !sender.getPlayer().isOnline()) {
            return;
        }

        final IChatter partner = this.getPartner(sender);

        if (partner == null) {
            sender.sendMessage(Messages.tlErr("partnerNotFound"));
            return;
        }

        if (sender.isIgnore(partner.getUniqueId())) {
            sender.sendMessage(Messages.tl("ignoredPartner", partner.getDisplayName()));
            return;
        }

        if (partner.isIgnore(sender.getUniqueId()) && !sender.hasPermission(Permission.IGNORE.getChildren("bypass"))) {
            sender.sendMessage(Messages.tl("ignoredSender", partner.getDisplayName()));
            return;
        }

        final ChatterChatConversationEvent event = new ChatterChatConversationEvent(sender, partner,
                this.getChatFormat(), message);

        this.instance.getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String format = this.instance.getFormatter().formatConversation(this, event.getFormat(), event.getMessage());

        sender.sendMessage(MessageFormat.format(format, Messages.tl("to"), partner.getDisplayName()));
        sender.setLastPartner(partner);

        partner.sendMessage(MessageFormat.format(format, Messages.tl("from"), sender.getDisplayName()));
        partner.setLastPartner(sender);

        if (!sender.hasPermission(Permission.SOCIAL_SPY.getChildren("exempt"))) {
            final String spy = Messages.tl("spyFormat", sender.getDisplayName(), partner.getDisplayName(), event.getMessage());

            for (final IChatter chatter : this.instance.getChatterManager().getChatters()) {
                if (chatter.equals(sender) || chatter.equals(partner)) {
                    continue;
                }

                if (chatter.isSocialSpy()) {
                    chatter.sendMessage(spy);
                }
            }
        }
    }

    /**
     * Builds the full name for conversation channels.
     *
     * <p>The full name is build of the uniqueId of both players concatenated with a underscore ("_").</p>
     *
     * <p>The order in which the chatters are specified as arguments does not matter as they are compared before.<br>
     * So buildConversationName(first, second) will return the same as buildConversationName(second, first).</p>
     *
     * @param first the first chatter to build the full name of the conversation.
     * @param second the second chatter to build the full name of the conversation.
     * @return the full name of the conversation for the given chatters.
     */
    static @NotNull String buildName(@NotNull final IChatter first, @NotNull final IChatter second) {
        if (first.getUniqueId().compareTo(second.getUniqueId()) >= 0) {
            return first.getUniqueId() + "_" + second.getUniqueId();
        } else {
            return second.getUniqueId() + "_" + first.getUniqueId();
        }
    }

    /**
     * Builds the short name for conversation channels.
     *
     * <p>The short name is build of the name of both players concatenated with a underscore ("_").</p>
     *
     * <p>The order in which the chatters are specified as arguments does not matter as they are compared before.<br>
     * So buildConversationName(first, second) will return the same as buildConversationName(second, first).</p>
     *
     * @param first the first chatter to build the short name of the conversation.
     * @param second the second chatter to build the short name of the conversation.
     * @return the short name of the conversation for the given chatters.
     */
    static @NotNull String buildShort(@NotNull final IChatter first, @NotNull final IChatter second) {
        if (first.getName().compareTo(second.getName()) >= 0) {
            return first.getName() + "_" + second.getName();
        } else {
            return second.getName() + "_" + first.getName();
        }
    }
}
