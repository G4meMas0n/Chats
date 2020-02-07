package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.chat.ChatFormatter;
import de.g4memas0n.Chats.chat.ChatPerformer;
import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
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
 * changed: February 2nd, 2020
 */
public final class ChannelManager implements IChannelManager {

    /**
     * the folder name for the channels directory.
     */
    private static final String DIRECTORY_NAME = "channels";

    private final Map<String, IChannel> channels;
    private final File directory;

    private final IChatFormatter formatter;
    private final IChatPerformer performer;

    private IChannel def;

    public ChannelManager(@NotNull final File parent) throws IOException, IllegalArgumentException {
        if (!parent.isDirectory()) {
            throw new IllegalArgumentException("Parent File must be a directory");
        }

        this.channels = new HashMap<>();
        this.directory = new File(parent, DIRECTORY_NAME);

        this.formatter = new ChatFormatter();
        this.performer = new ChatPerformer(this.formatter);

        this.reload();
    }

    @Override
    public @NotNull IChatFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public @NotNull IChatPerformer getPerformer() {
        return this.performer;
    }

    // Default Channel Methods:
    @Override
    public @NotNull IChannel getDefault() {
        if (this.def == null) {
            throw new IllegalStateException("Missing default channel");
        }

        return this.def;
    }

    public boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException("Channel must be persistent");
        }

        if (channel.equals(this.def)) {
            return false;
        }

        if (!this.channels.containsKey(channel.getFullName())) {
            this.channels.put(channel.getFullName(), channel);
        }

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

    public boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (channel.equals(this.def)) {
            throw new IllegalArgumentException("Default channel can not be removed");
        }

        if (!this.channels.containsKey(channel.getFullName())) {
            return false;
        }

        for (IChatter current : channel.getChatters()) {
            current.removeChannel(channel);
        }

        if (channel.isPersist()) {
            channel.delete();
        }

        return true;
    }

    @Override
    public boolean hasChannel(@NotNull final String fullName) {
        return this.channels.containsKey(fullName);
    }

    @Override
    public boolean hasPersist(@NotNull final String fullName) {
        if (!this.channels.containsKey(fullName)) {
            return false;
        }

        return this.channels.get(fullName).isPersist();
    }

    @Override
    public boolean hasConversation(@NotNull final String fullName) {
        if (!this.channels.containsKey(fullName)) {
            return false;
        }

        return this.channels.get(fullName).isConversation();
    }

    @Override
    public @NotNull IChannel create(@NotNull final String fullName) throws IllegalArgumentException {
        return new StandardChannel(this.formatter, this.performer, fullName);
    }

    @Override
    public @NotNull IChannel createPersist(@NotNull final String fullName) throws IllegalArgumentException {
        final File file = this.getFile(fullName);

        if (file.exists()) {
            throw new IllegalArgumentException("Storage file for persist channel '" + fullName + "' already exists");
        }

        return new PersistChannel(this.formatter, this.performer, this.getFile(fullName));
    }

    @Override
    public @NotNull IChannel createConversation(@NotNull final IChatter first, @NotNull final IChatter second) {
        return new ConversationChannel(this.formatter, this.performer, first, second);
    }

    public void reload() throws IOException {
        final File[] files = this.directory.listFiles(File::isFile);

        if (files == null) {
            throw new IOException("Failed to load files from directory " + this.directory.getName());
        }

        this.channels.clear();

        for (final File current : files) {
            try {
                final IChannel channel = new PersistChannel(this.formatter, this.performer, current);

                channel.reload();

                this.addChannel(channel);
            } catch (IllegalArgumentException ignored) {
                // This should never be the case here.
            }
        }

        if (this.def != null) {
            try {
                IChannel channel;

                if (this.channels.containsKey(this.def.getFullName())) {
                    channel = this.channels.get(this.def.getFullName());
                } else {
                    channel = this.createPersist(this.def.getFullName());

                    channel.save();
                }

                this.setDefault(channel);
            } catch (IllegalArgumentException ignored) {
                // This should never be the case here.
            }
        }
    }

    private @NotNull File getFile(@NotNull final String name) {
        return new File(this.directory, name + ".yml");
    }
}
