package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * Representation of a Chatter Manager, implements the {@link IChannelManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: November 14th, 2019
 * last change: November 15th, 2019
 */
public final class ChannelManager implements IChannelManager {

    private final Map<String, IChannel> channels;
    private final IChannelStorage storage;
    private String defaultChannel;

    public ChannelManager(@NotNull final File directory) {
        this.channels = new HashMap<>();
        this.storage = new YAMLChannelStorage(directory);
    }

    @Override
    public @NotNull IChannelStorage getChannelStorage() {
        return this.storage;
    }

    @Override
    public @NotNull IChannel getDefaultChannel() {
        return this.channels.get(this.defaultChannel);
    }

    @Override
    public boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!this.hasChannel(channel.getFullName())) {
            throw new IllegalArgumentException("Invalid channel name! Channel must be registered in this manager.");
        }

        if (!channel.isPersistChannel()) {
            throw new IllegalArgumentException("Invalid channel type! Channel must be a persist channel.");
        }

        if (this.defaultChannel.equals(channel.getFullName())) {
            return false;
        }

        this.defaultChannel = channel.getFullName();
        return true;
    }

    @Override
    public boolean setDefaultChannel(@NotNull final String fullName) throws IllegalArgumentException {
        IChannel channel = this.channels.get(fullName);

        if (channel == null) {
            throw new IllegalArgumentException("Invalid channel name! Channel must be registered in this manager.");
        }

        return this.setDefaultChannel(channel);
    }

    @Override
    public boolean hasChannel(@NotNull final String fullName) {
        return this.channels.containsKey(fullName);
    }

    @Override
    public boolean hasPersistChannel(@NotNull final String fullName) {
        if (this.channels.containsKey(fullName)) {
            return this.channels.get(fullName).isPersistChannel();
        }

        return false;
    }

    @Override
    public boolean hasConversionChannel(@NotNull final String fullName) {
        if (this.channels.containsKey(fullName)) {
            return this.channels.get(fullName).isConversionChannel();
        }

        return false;
    }

    @Override
    public @NotNull Collection<IChannel> getChannels() {
        return this.channels.values();
    }

    @Override
    public @Nullable IChannel getChannel(@NotNull final String fullName) {
        return this.channels.get(fullName);
    }

    @Override
    public @Nullable IChannel getConversionChannel(@NotNull final String fullName) {
        IChannel channel = this.channels.get(fullName);
        return channel != null && channel.isConversionChannel() ? channel : null;
    }

    @Override
    public boolean addChannel(@NotNull final IChannel channel) {
        if (this.hasChannel(channel.getFullName())) {
            return false;
        }

        this.channels.put(channel.getFullName(), channel);
        return true;
    }

    @Override
    public boolean removeChannel(@NotNull final String fullName) throws IllegalArgumentException {
        if (this.defaultChannel.equals(fullName)) {
            throw new IllegalArgumentException("Invalid channel name! The default channel can not be removed");
        }

        final IChannel channel = this.channels.remove(fullName);

        if (channel == null) {
            return false;
        }

        for (IChatter current : new HashSet<>(channel.getChatters())) {
            channel.removeChatter(current);
            current.removeChannel(channel);
        }

        if (channel.isPersistChannel()) {
            return this.storage.delete(channel);
        } else {
            return true;
        }
    }

    @Override
    public boolean updateName(@NotNull final String oldName, @NotNull final String newName) {
        if (this.hasChannel(newName)) {
            return false;
        }

        final IChannel channel = this.channels.get(oldName);

        if (channel == null) {
            return false;
        }

        if (channel.setFullName(newName)) {
            this.channels.remove(oldName);
            this.channels.put(newName, channel);
            return true;
        } else {
            return false;
        }
    }
}
