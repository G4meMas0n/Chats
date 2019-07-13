package de.g4memas0n.Chats.chatters;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.channels.IChannel;
import de.g4memas0n.Chats.events.ChatterChangedChannelEvent;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

public final class Chatter extends AbstractChatter implements IChatter {
    private IChannel activeChannel;
    private IChannel lastPersistChannel;
    private IChatter lastConversionPartner;
    private Set<IChannel> channels;
    private Set<UUID> ignoredPlayers;

    public Chatter(@NotNull final Player player,
                   @NotNull final IChannel defaultChannel) {
        super(player);
        this.activeChannel = defaultChannel;
        this.lastPersistChannel = null;
        this.lastConversionPartner = null;
        this.channels = new HashSet<>();
        this.channels.add(defaultChannel);
        this.ignoredPlayers = new HashSet<>();
    }

    public Chatter(@NotNull final Player player,
                   @NotNull final String activeChannel,
                   @NotNull final Collection<IChannel> channels,
                   @NotNull final Collection<UUID> ignoredPlayers) throws IllegalArgumentException {
        super(player);

        for (IChannel current : channels) {
            if (current.getFullName().equals(activeChannel)) {
                this.activeChannel = current;
            }
        }

        if (this.activeChannel == null) {
            throw new IllegalArgumentException("Chatter isn't in the given active channel.");
        }

        this.lastPersistChannel = null;
        this.lastConversionPartner = null;
        this.channels = new HashSet<>();
        this.channels.addAll(channels);
        this.ignoredPlayers = new HashSet<>();
        this.ignoredPlayers.addAll(ignoredPlayers);
    }

    // Active Channel Methods:
    @Override
    public @NotNull IChannel getActiveChannel() {
        return this.activeChannel;
    }

    @Override
    public boolean setActiveChannel(@NotNull final IChannel channel) {
        return this.setActiveChannel(channel, false);
    }

    private boolean setActiveChannel(@NotNull final IChannel channel, final boolean removedActiveChannel) {
        if (this.activeChannel.equals(channel)) {
            return false;
        }

        if (!this.channels.contains(channel)) {
            this.addChannel(channel);
        }

        ChatterChangedChannelEvent event = new ChatterChangedChannelEvent(this, this.activeChannel);

        if (removedActiveChannel) {
            this.lastPersistChannel = null;
        } else {
            if (this.activeChannel.isPersistChannel()) {
                this.lastPersistChannel = this.activeChannel;
            }
        }

        this.activeChannel = channel;

        //TODO: Inform Message

        if (Chats.getInstance() != null) {
            Chats.getInstance().getPluginManager().callEvent(event);
        }

        return true;
    }


    // Last Sources Methods:
    @Override
    @Nullable
    public IChannel getLastPersistChannel() {
        return this.lastPersistChannel;
    }

    @Override
    @Nullable
    public IChatter getLastConversionPartner() {
        return this.lastConversionPartner;
    }

    @Override
    public boolean setLastConversionPartner(@NotNull final IChatter chatter) throws IllegalArgumentException {
        if (chatter.equals(this)) {
            throw new IllegalArgumentException("this chatter can not have himself as the last conversion partner");
        }

        if (this.lastConversionPartner.equals(chatter)) {
            return false;
        }

        this.lastConversionPartner = chatter;
        return true;
    }

    // Channels Collection Methods:
    @Override
    @NotNull
    public Collection<IChannel> getChannels() {
        return this.channels;
    }

    @Override
    public boolean addChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel)) {
            return false;
        }

        this.channels.add(channel);

        if (!channel.getChatters().contains(this)) {
            channel.addChatter(this);
        }

        return true;
    }

    @Override
    public boolean removeChannel(@NotNull final IChannel channel) {
        if (!this.channels.contains(channel)) {
            return false;
        }

        if (this.activeChannel.equals(channel)) {
            this.setActiveChannel(this.lastPersistChannel, true);
        }

        this.channels.remove(channel);

        if (channel.getChatters().contains(this)) {
            channel.removeChatter(this);
        }

        return true;
    }

    @Override
    @NotNull
    public Collection<UUID> getIgnoredChatters() {
        return this.ignoredPlayers;
    }

    @Override
    public boolean addIgnoredChatter(@NotNull final UUID playerUUID) {
        if (this.ignoredPlayers.contains(playerUUID)) {
            return false;
        }

        this.ignoredPlayers.add(playerUUID);
        return true;
    }

    @Override
    public boolean removeIgnoredChatter(@NotNull final UUID playerUUID) {
        if (!this.ignoredPlayers.contains(playerUUID)) {
            return false;
        }

        this.ignoredPlayers.remove(playerUUID);
        return true;
    }

    @Override
    public boolean isIgnoring(@NotNull final UUID playerUUID) {
        return this.ignoredPlayers.contains(playerUUID);
    }
}