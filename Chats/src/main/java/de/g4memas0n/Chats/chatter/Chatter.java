package de.g4memas0n.Chats.chatter;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.event.ChatterChangedChannelEvent;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Representation of a chatter, implements the {@link IChatter} interface, extends {@link AbstractChatter}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: November 19th, 2019
 */
public final class Chatter extends AbstractChatter implements IChatter {

    private final IChatterStorage storage;

    private IChannel activeChannel;
    private IChannel lastPersistChannel;
    private Set<IChatter> lastConversionPartner;
    private Set<IChannel> channels;
    private Set<UUID> ignores;

    protected Chatter(@NotNull final Player player,
                      @NotNull final IChatterStorage storage) {
        super(player);
        this.storage = storage;
        this.channels = new HashSet<>();
        this.ignores = new HashSet<>();

        this.lastPersistChannel = null;
        this.lastConversionPartner = null;
    }

    // Active Channel Methods:
    @Override
    public @NotNull IChannel getActiveChannel() throws IllegalStateException {
        if (this.activeChannel == null) {
            throw new IllegalStateException("Missing active channel for chatter: " + this.player.getName()
                    + " with uuid: " + this.player.getUniqueId().toString());
        }

        return this.activeChannel;
    }

    @Override
    public boolean setActiveChannel(@NotNull final IChannel channel) {
        if (this.activeChannel.equals(channel)) {
            return false;
        }

        if (!this.channels.contains(channel)) {
            this.addChannel(channel);
        }

        IChannel from = this.activeChannel;

        if (this.lastPersistChannel.equals(channel)) {
            this.lastPersistChannel = null;
        } else {
            if (from.isPersistChannel()) {
                this.lastPersistChannel = from;
            }
        }

        this.activeChannel = channel;

        IChats instance = Chats.getInstance();
        if (instance != null) {
            ChatterChangedChannelEvent event = new ChatterChangedChannelEvent(this, from);

            instance.getPluginManager().callEvent(event);
        }

        this.player.sendMessage(""); //TODO Add translated active channel changed message.
        this.update();
        return true;
    }

    void _setActiveChannel(@NotNull final IChannel channel) {
        if (this.activeChannel.equals(channel)) {
            return;
        }

        if (!this.channels.contains(channel)) {
            this._addChannel(channel);
        }

        this.activeChannel = channel;
    }

    // Last Sources Methods:
    @Override
    public @Nullable IChannel getLastPersistChannel() {
        return this.lastPersistChannel;
    }

    @Override
    public @Nullable Set<IChatter> getLastConversionPartner() {
        return this.lastConversionPartner;
    }

    @Override
    public boolean setLastConversionPartner(@NotNull final Set<IChatter> chatters) throws IllegalArgumentException {
        chatters.remove(this);

        if (chatters.isEmpty()) {
            throw new IllegalArgumentException("the last conversion partners can not be empty.");
        }

        if (this.lastConversionPartner.equals(chatters)) {
            return false;
        }

        this.lastConversionPartner = chatters;
        return true;
    }

    // Channels Collection Methods:
    @Override
    public @NotNull Set<IChannel> getChannels() {
        return this.channels;
    }

    @Override
    public boolean addChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel)) {
            return false;
        }

        this.channels.add(channel);

        if (!channel.getChatters().contains(this)) {
            channel.performAnnounce(""); //TODO Add translated channel join message.
            channel.addChatter(this);
        }

        if (channel.isPersistChannel()) {
            this.update();
        }
        return true;
    }

    void _addChannel(@NotNull final IChannel channel) {
        if (this.channels.contains(channel)) {
            return;
        }

        this.channels.add(channel);

        if (!channel.getChatters().contains(this)) {
            channel.addChatter(this);
        }
    }

    @Override
    public boolean removeChannel(@NotNull final IChannel channel) {
        if (!this.channels.contains(channel)) {
            return false;
        }

        if (this.activeChannel.equals(channel)) {
            if (this.lastPersistChannel == null) {
                this.setActiveChannel(this.storage.getChannelManager().getDefaultChannel());
            }

            this.setActiveChannel(this.lastPersistChannel);
        }

        this.channels.remove(channel);

        if (channel.getChatters().contains(this)) {
            channel.removeChatter(this);
            channel.performAnnounce(""); //TODO Add translated channel leave message.
        }

        if (channel.isPersistChannel()) {
            this.update();
        }
        return true;
    }

    void _removeChannel(@NotNull final IChannel channel) {
        if (!this.channels.contains(channel)) {
            return;
        }

        this.channels.remove(channel);

        if (channel.getChatters().contains(this)) {
            channel.removeChatter(this);
        }
    }

    @Override
    public @NotNull Set<UUID> getIgnores() {
        return this.ignores;
    }

    void _setIgnores(@NotNull final Set<UUID> chatters) {
        if (this.ignores.equals(chatters)) {
            return;
        }

        this.ignores = chatters;
    }

    @Override
    public boolean addIgnores(@NotNull final UUID playerUUID) {
        if (this.ignores.contains(playerUUID)) {
            return false;
        }

        this.ignores.add(playerUUID);
        this.player.sendMessage(""); //TODO Add translated now ignoring message.
        this.update();
        return true;
    }

    @Override
    public boolean removeIgnores(@NotNull final UUID playerUUID) {
        if (!this.ignores.contains(playerUUID)) {
            return false;
        }

        this.ignores.remove(playerUUID);
        this.player.sendMessage(""); //TODO Add translated now more ignoring message.
        this.update();
        return true;
    }

    @Override
    public boolean isIgnoring(@NotNull final UUID playerUUID) {
        return this.ignores.contains(playerUUID);
    }

    @Override
    public int compareTo(@NotNull final IChatter chatter) {
        return this.player.getUniqueId().compareTo(chatter.getPlayer().getUniqueId());
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final IChatter chatter = (Chatter) object;
        return this.player.equals(chatter.getPlayer());
    }

    @Override
    public final int hashCode() {
        final int prime = 37;
        int result = 4;

        result = prime * result + this.player.hashCode();

        return result;
    }

    private void update() {
        this.storage.update(this);
    }
}
