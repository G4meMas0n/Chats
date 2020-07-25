package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.YamlStorageFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * Implementation of a channel manager that holds and manage currently existing channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ChannelManager implements IChannelManager {

    /**
     * the folder name for the channels directory.
     */
    private static final String DIRECTORY_NAME = "channels";

    private final Map<String, IChannel> channels;

    private final Chats instance;
    private final File directory;

    private IChannel def;

    public ChannelManager(@NotNull final Chats instance) {
        this.instance = instance;
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);

        if (this.directory.mkdirs()) {
            this.instance.getLogger().debug(String.format("Directory '%s' does not exist. Creating it...", this.directory));
        }

        this.channels = new HashMap<>();
    }

    // Default Channel Methods:
    @Override
    public synchronized @NotNull IChannel getDefault() {
        if (this.def == null) {
            throw new IllegalStateException(String.format("Default channel '%s' is missing.",
                    this.instance.getSettings().getDefaultChannel()));
        }

        return this.def;
    }

    @Override
    public synchronized boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException(String.format("Channel '%s' is not persistent", channel.getFullName()));
        }

        if (channel.equals(this.def)) {
            return false;
        }

        final String key = channel.getFullName().toLowerCase();

        if (!this.channels.containsKey(key)) {
            this.channels.put(key, channel);
        }

        this.def = channel;
        return true;
    }

    // Channel Collection Methods:
    @Override
    public synchronized @NotNull Set<IChannel> getChannels() {
        return new HashSet<>(this.channels.values());
    }

    @Override
    public synchronized @Nullable IChannel getChannel(@NotNull final String fullName) {
        return this.channels.get(fullName.toLowerCase());
    }

    @Override
    public synchronized @Nullable IChannel getPersist(@NotNull final String fullName) {
        final IChannel persist = this.channels.get(fullName.toLowerCase());

        if (persist == null || !persist.isPersist()) {
            return null;
        }

        return persist;
    }

    @Override
    public synchronized @Nullable IChannel getStandard(@NotNull final String fullName) {
        final IChannel standard = this.channels.get(fullName.toLowerCase());

        if (standard == null || !standard.isStandard()) {
            return null;
        }

        return standard;
    }

    @Override
    public synchronized @NotNull IChannel getConversation(@NotNull final IChatter first,
                                                          @NotNull final IChatter second) {
        final String key = ConversationChannel.buildName(first, second);

        if (this.channels.containsKey(key)) {
            final IChannel conversation = this.channels.get(key);

            if (conversation.getMembers().size() == 2) {
                return conversation;
            }
        }

        final IChannel conversation = new ConversationChannel(this.instance, first, second);

        this.channels.put(key, conversation);

        return conversation;
    }

    @Override
    public synchronized boolean addChannel(@NotNull final IChannel channel) {
        final String key = channel.getFullName().toLowerCase();

        if (this.channels.containsKey(key)) {
            return false;
        }

        this.channels.put(key, channel);
        return true;
    }

    public synchronized @Nullable PersistChannel addPersist(@NotNull final String fullName) throws IllegalArgumentException {
        if (this.channels.containsKey(fullName.toLowerCase())) {
            return null;
        }

        final PersistChannel persist = new PersistChannel(this.instance, new YamlStorageFile(this.directory, fullName));

        this.channels.put(persist.getFullName().toLowerCase(), persist);

        return persist;
    }

    public synchronized @Nullable StandardChannel addStandard(@NotNull final String fullName) throws IllegalArgumentException {
        if (this.channels.containsKey(fullName.toLowerCase())) {
            return null;
        }

        final StandardChannel standard = new StandardChannel(this.instance, fullName);

        this.channels.put(standard.getFullName().toLowerCase(), standard);

        return standard;
    }

    @Override
    public synchronized boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (channel.equals(this.def)) {
            throw new IllegalArgumentException(String.format("Channel '%s' is default and can not be removed",
                    channel.getFullName()));
        }

        final String key = channel.getFullName().toLowerCase();

        if (!this.channels.containsKey(key) || !this.channels.remove(key, channel)) {
            return false;
        }

        for (final IChatter member : channel.getMembers()) {
            member.leaveChannel(channel, true);
        }

        // Check if the channel is backed by a storage. Delete it when true.
        if (channel instanceof IStorageHolder) {
            this.instance.runStorageTask(((IStorageHolder) channel)::delete);
        }

        return true;
    }

    @Override
    public synchronized boolean hasChannels() {
        return !this.channels.isEmpty();
    }

    @Override
    public synchronized boolean hasChannel(@NotNull final String fullName) {
        return this.channels.containsKey(fullName.toLowerCase());
    }

    @Override
    public synchronized void load() {
        this.instance.getLogger().info("Loading channels...");

        this.channels.clear();
        this.def = null;

        final Set<IStorageHolder> loading = new HashSet<>();
        final String defName = this.instance.getSettings().getDefaultChannel();

        for (final File file : this.directory.listFiles(File::isFile)) {
            try {
                final PersistChannel persist = new PersistChannel(this.instance, new YamlStorageFile(file));

                // Check if the current channel name equals the name of the default channel.
                if (persist.getFullName().equalsIgnoreCase(defName)) {
                    this.def = persist;
                }

                loading.add(persist);

                this.channels.put(persist.getFullName().toLowerCase(), persist);
            } catch (IllegalArgumentException ignored) {
                // Directory can contain invalid storage files, just ignore them.
            }
        }

        final Future<?> task = this.instance.runStorageTask(() -> loading.forEach(IStorageHolder::load));

        // Check if the default channel is still missing.
        if (this.def == null) {
            this.instance.getLogger().warning(String.format("Detected missing default channel '%s'. Creating it...", defName));

            final PersistChannel def = new PersistChannel(this.instance, new YamlStorageFile(this.directory, defName));

            this.instance.runStorageTask(def::save);
            this.channels.put(def.getFullName().toLowerCase(), def);
            this.def = def;
        }

        try {
            task.get();
        } catch (ExecutionException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
        } catch (InterruptedException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        this.instance.getLogger().info("Channels have been loaded.");

        //TODO: Updating online chatters to the new loaded channels.
    }

    @Override
    public synchronized void save() {
        this.instance.getLogger().info("Saving channels...");

        final Set<IStorageHolder> saving = new HashSet<>();

        for (final IChannel channel : this.channels.values()) {
            if (!channel.isPersist()) {
                continue;
            }

            if (channel instanceof IStorageHolder) {
                saving.add((IStorageHolder) channel);
            }
        }

        try {
            this.instance.runStorageTask(() -> saving.forEach(IStorageHolder::save)).get();
        } catch (ExecutionException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Storage task has thrown an unexpected exception", ex);
        } catch (InterruptedException ex) {
            this.instance.getLogger().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        this.instance.getLogger().info("Channels have been saved.");
    }
}
