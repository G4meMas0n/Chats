package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.event.chatter.ChatterChatConversationEvent;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.Set;
import java.util.UUID;

/**
 * Implementation of a conversation channel that handles conversation actions between chatters (players).
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ConversationChannel extends StandardChannel {

    public ConversationChannel(@NotNull final IChats instance,
                               @NotNull final IChatter first,
                               @NotNull final IChatter second) throws IllegalArgumentException {
        super(instance, buildName(first, second));

        super.setShortName(buildShort(first, second));
        super.setColor(ChatColor.LIGHT_PURPLE);

        this.addMember(first);
        this.addMember(second);
    }

    // Channel Properties Methods:
    @Override
    public boolean setShortName(@Nullable final String shortName) {
        return false;
    }

    @Override
    public boolean setColor(@Nullable final ChatColor color) {
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
    public boolean isCrossWorld() {
        return true;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
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

    @Override
    public @NotNull String getAnnounceFormat() {
        return this.getInstance().getFormatter().getAnnounceFormat();
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    public @NotNull String getBroadcastFormat() {
        return this.getInstance().getFormatter().getBroadcastFormat();
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.getInstance().getFormatter().getConversationFormat();
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

    public synchronized boolean setMember(@NotNull final IChatter chatter, final boolean member) {
        if (!this.getFullName().contains(chatter.getUniqueId().toString())) {
            return false;
        }

        if (member) {
            return super.setMember(chatter, true);
        }

        if (super.setMember(chatter, false)) {
            // Ensures that the IChannelManager#removeChannel() method is always called synchronous.
            this.getInstance().runSyncTask(() -> this.getInstance().getChannelManager().removeChannel(this));
            return true;
        }

        return false;
    }

    @Override
    public synchronized boolean addMember(@NotNull final IChatter chatter) {
        if (this.setMember(chatter, true)) {
            if (!chatter.hasChannel(this)) {
                chatter.joinChannel(this);
            }

            return true;
        }

        return false;
    }

    @Override
    public synchronized boolean removeMember(@NotNull final IChatter chatter) {
        if (this.setMember(chatter, false)) {
            if (chatter.hasChannel(this)) {
                chatter.leaveChannel(this);
            }

            return true;
        }

        return false;
    }

    @Override
    public boolean kickMember(@NotNull final IChatter member) {
        return false;
    }

    @Override
    public boolean banMember(@NotNull final IChatter member) {
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
    public @NotNull Set<UUID> getBans() {
        return Collections.emptySet();
    }

    @Override
    public boolean setBans(@NotNull final Set<UUID> bans) {
        return false;
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        return false;
    }

    @Override
    public boolean isBanned(@NotNull final UUID uniqueId) {
        return false;
    }

    @Override
    public @NotNull Set<UUID> getModerators() {
        return Collections.emptySet();
    }

    @Override
    public boolean setModerators(@NotNull final Set<UUID> moderators) {
        return false;
    }

    @Override
    public boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator) {
        return false;
    }

    @Override
    public boolean isModerator(@NotNull final UUID uniqueId) {
        return false;
    }

    @Override
    public @NotNull Set<UUID> getMutes() {
        return Collections.emptySet();
    }

    @Override
    public boolean setMutes(@NotNull final Set<UUID> mutes) {
        return false;
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        return false;
    }

    @Override
    public boolean isMuted(@NotNull final UUID uniqueId) {
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
                this.getInstance().getFormatter().getConversationFormat(), message, !Bukkit.isPrimaryThread());

        this.getInstance().getServer().getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        final String format = this.getInstance().getFormatter().formatConversation(this, event.getFormat(), event.getMessage());

        sender.sendMessage(MessageFormat.format(format, Messages.tl("to"), partner.getDisplayName()));
        sender.setLastPartner(partner);

        partner.sendMessage(MessageFormat.format(format, Messages.tl("from"), sender.getDisplayName()));
        partner.setLastPartner(sender);

        if (!sender.hasPermission(Permission.SOCIAL_SPY.getChildren("exempt"))) {
            final String spy = Messages.tl("spyFormat", sender.getDisplayName(), partner.getDisplayName(), event.getMessage());

            for (final IChatter chatter : this.getInstance().getChatterManager().getChatters()) {
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
        if (first.compareTo(second) >= 0) {
            return first.getPlayer().getUniqueId() + "_" + second.getPlayer().getUniqueId();
        } else {
            return second.getPlayer().getUniqueId() + "_" + first.getPlayer().getUniqueId();
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
        if (first.compareTo(second) >= 0) {
            return first.getPlayer().getName() + "_" + second.getPlayer().getName();
        } else {
            return second.getPlayer().getName() + "_" + first.getPlayer().getName();
        }
    }
}
