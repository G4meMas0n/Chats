package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.chatter.IChatter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Representation of a Chatter Manager, implements the {@link IChannelManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: November 14th, 2019
 * changed: March 9th, 2020
 */
public final class ChannelManager implements IChannelManager {

    /**
     * the folder name for the channels directory.
     */
    private static final String DIRECTORY_NAME = "channels";

    private final Map<String, IChannel> channels;
    private final IChats instance;
    private final File directory;

    private IChannel def;

    public ChannelManager(@NotNull final IChats instance) {
        this.channels = new HashMap<>();
        this.instance = instance;
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);
    }

    // Default Channel Methods:
    @Override
    public @NotNull IChannel getDefault() {
        if (this.def == null) {
            throw new IllegalStateException("Missing default channel");
        }

        return this.def;
    }

    @Override
    public boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException("Channel is not persistent");
        }

        if (channel.equals(this.def)) {
            return false;
        }

        if (!this.channels.containsKey(channel.getFullName())) {
            this.channels.put(channel.getFullName(), channel);
        }

        this.instance.getSettings().setDefaultChannel(channel.getFullName());
        this.def = channel;
        return true;
    }

    // Channel Collection Methods:
    @Override
    public @NotNull Set<IChannel> getChannels() {
        return new HashSet<>(this.channels.values());
    }

    @Override
    public @Nullable IChannel getChannel(@NotNull final String fullName) {
        return this.channels.get(fullName);
    }

    @Override
    public boolean addChannel(@NotNull final IChannel channel) {
        if (this.channels.containsKey(channel.getFullName())) {
            return false;
        }

        this.channels.put(channel.getFullName(), channel);
        return true;
    }

    @Override
    public boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (channel.equals(this.def)) {
            throw new IllegalArgumentException("Default channel can not be removed");
        }

        if (!this.channels.containsKey(channel.getFullName())) {
            return false;
        }

        for (IChatter current : channel.getMembers()) {
            current.leaveChannel(channel);
        }

        if (channel instanceof PersistChannel) {
            ((PersistChannel) channel).delete();
        }

        return true;
    }

    @Override
    public boolean hasChannel(@NotNull final String fullName) {
        return this.channels.containsKey(fullName);
    }

    @Override
    public @NotNull IStorageFile getStorageFile(@NotNull final String fullName) {
        return new YamlStorageFile(new File(this.directory, fullName));
    }

    @Override
    public void reload() throws IOException {
        final File[] files = this.directory.listFiles(File::isFile);

        if (files == null) {
            throw new IOException("Unable to load files from directory: " + this.directory.getName());
        }

        this.channels.clear();

        for (final File current : files) {
            try {
                this.addChannel(new PersistChannel(this.instance.getFormatter(), new YamlStorageFile(current)));
            } catch (IllegalArgumentException ignored) {
                // This should never be the case here.
            }
        }

        try {
            final String name = this.instance.getSettings().getDefaultChannel();

            if (this.channels.containsKey(name)) {
                this.setDefault(this.channels.get(name));
            } else {
                final PersistChannel def = new PersistChannel(this.instance.getFormatter(), this.getStorageFile(name));

                this.setDefault(def);

                def.save();
            }
        } catch (IllegalArgumentException ex) {
            // This should never be the case here.
        }

        this.instance.getChatterManager().reload();
    }
}
