package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.util.logging.Log;
import de.g4memas0n.chats.util.type.ChannelType;
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
 * Representation of a Chatter Manager, implements the {@link IChannelManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: November 14th, 2019
 * changed: June 17th, 2020
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
        this.directory = new File(instance.getDataFolder(), DIRECTORY_NAME);

        if (this.directory.mkdirs()) {
            Log.getPlugin().debug(String.format("Directory '%s' does not exist. Creating it...", this.directory));
        }

        this.instance = instance;
        this.channels = new HashMap<>();
    }

    // Default Channel Methods:
    @Override
    public synchronized @NotNull IChannel getDefault() {
        if (this.def == null) {
            throw new IllegalStateException(String.format("Missing default channel: %s",
                    this.instance.getSettings().getDefaultChannel()));
        }

        return this.def;
    }

    @Override
    public synchronized boolean setDefault(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersist()) {
            throw new IllegalArgumentException(String.format("Channel is not persistent: %s", channel.getFullName()));
        }

        if (channel.equals(this.def)) {
            return false;
        }

        final String key = channel.getFullName().toLowerCase();

        if (!this.channels.containsKey(key)) {
            this.channels.put(key, channel);
        }

        this.def = channel;

        Log.getPlugin().debug("Default channel has been set to: " + channel.getFullName());
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
            int partners = 0;

            for (final IChatter member : conversation.getMembers()) {
                if (conversation.getFullName().contains(member.getUniqueId().toString())) {
                    partners++;
                }
            }

            if (partners == 2) {
                return conversation;
            }
        }

        final IChannel conversation = new ConversationChannel(this.instance, first, second);

        this.addChannel(conversation);

        return conversation;
    }

    public synchronized @Nullable IChannel addChannel(@NotNull final String fullName,
                                                      @NotNull final ChannelType type) throws IllegalArgumentException {
        if (this.channels.containsKey(fullName.toLowerCase())) {
            return null;
        }

        if (type == ChannelType.PERSIST) {
            final IChannel persist = new PersistChannel(this.instance, new YamlStorageFile(this.directory, fullName));

            this.addChannel(persist);

            return persist;
        }

        if (type == ChannelType.STANDARD) {
            final IChannel standard = new StandardChannel(this.instance, fullName);

            this.addChannel(standard);

            return standard;
        }

        return null;
    }

    @Override
    public synchronized boolean addChannel(@NotNull final IChannel channel) {
        final String key = channel.getFullName().toLowerCase();

        if (this.channels.containsKey(key)) {
            return false;
        }

        this.channels.put(key, channel);

        Log.getPlugin().debug("Channel has been added to the channel manager: " + channel.getFullName());
        return true;
    }

    @Override
    public synchronized boolean removeChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (channel.equals(this.def)) {
            throw new IllegalArgumentException(String.format("Default channel can not be removed: %s",
                    channel.getFullName()));
        }

        final String key = channel.getFullName().toLowerCase();

        if (!this.channels.containsKey(key) || !this.channels.remove(key, channel)) {
            return false;
        }

        for (final IChatter current : channel.getMembers()) {
            // The current chatter is first removed from the channel with the #setMember method and then the channel
            // is removed from the current chatter to removing all members silently.
            // To remove all members non silently just remove or comment the #setMember method.
            channel.setMember(current, false);
            current.leaveChannel(channel);
        }

        // Check if the channel is backed by a storage. Delete it when true.
        if (channel instanceof IStorageHolder) {
            this.instance.runStorageTask(((IStorageHolder) channel)::delete);
        }

        Log.getPlugin().debug("Channel has been removed from the channel manager: " + channel.getFullName());
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
        Log.getPlugin().info("Loading channels...");

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
                // Can be ignored, because the current file can not be a valid channel storage file.
                Log.getPlugin().warning(String.format("Detected invalid storage file '%s'. Ignoring it...", file));
            }
        }

        // Check if the default channel is still missing.
        if (this.def == null) {
            Log.getPlugin().warning(String.format("Detected missing default channel '%s'. Creating it...", defName));

            final PersistChannel def = new PersistChannel(this.instance, new YamlStorageFile(this.directory, defName));

            this.instance.runStorageTask(def::save);
            this.channels.put(def.getFullName().toLowerCase(), def);
            this.def = def;
        }

        final Future<?> task = this.instance.runStorageTask(() -> loading.forEach(IStorageHolder::load));

        try {
            task.get();
        } catch (ExecutionException ex) {
            Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
        } catch (InterruptedException ex) {
            Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        Log.getPlugin().info("Channels have been loaded.");

        //TODO: Updating online chatters to the new loaded channels.
    }

    @Override
    public synchronized void save() {
        Log.getPlugin().info("Saving channels...");

        final Set<IStorageHolder> saving = new HashSet<>();

        for (final IChannel channel : this.channels.values()) {
            if (!channel.isPersist()) {
                continue;
            }

            if (channel instanceof IStorageHolder) {
                saving.add((IStorageHolder) channel);
            }
        }

        final Future<?> task = this.instance.runStorageTask(() -> saving.forEach(IStorageHolder::save));

        try {
            task.get();
        } catch (ExecutionException ex) {
            Log.getPlugin().log(Level.SEVERE, "Storage task has thrown an unexpected exception: ", ex);
        } catch (InterruptedException ex) {
            Log.getPlugin().log(Level.SEVERE, "Thread got interrupted while waiting for storage task to terminate.", ex);
        }

        Log.getPlugin().info("Channels have been saved.");
    }
}
