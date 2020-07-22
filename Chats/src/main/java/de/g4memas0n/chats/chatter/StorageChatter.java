package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.YamlStorageFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * Abstract representation of a chatter storage file.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class StorageChatter implements IStorageHolder {

    protected final Chats instance;
    protected final YamlStorageFile storage;

    protected Future<?> saveTask;

    protected StorageChatter(@NotNull final Chats instance,
                             @NotNull final YamlStorageFile storage) {
        this.instance = instance;
        this.storage = storage;
    }

    protected synchronized void _delayedSave() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            return;
        }

        this.saveTask = this.instance.scheduleStorageTask(this::save);
    }

    protected final @Nullable UUID _getUniqueId() {
        return this.storage.getUniqueId("uuid");
    }

    protected final void _setUniqueId(@NotNull final UUID uniqueId) {
        this.storage.set("uuid", uniqueId.toString());
    }

    protected final @Nullable String _getLastName() {
        return this.storage.getString("last.name");
    }

    protected final void _setLastName(@NotNull final String name) {
        this.storage.set("last.name", name);
    }

    protected final long _getLastPlayed() {
        return this.storage.getLong("last.played", -1);
    }

    protected final void _setLastPlayed(final long millis) {
        this.storage.set("last.played", millis);
    }

    protected final boolean _getMuted() {
        if (!this.storage.contains("muted")) {
            this._setMuted(false);
            this._delayedSave();
        }

        return this.storage.getBoolean("muted", false);
    }

    protected final void _setMuted(final boolean muted) {
        this.storage.set("muted", muted);
    }

    protected final boolean _getSocialSpy() {
        if (!this.storage.contains("social-spy")) {
            this._setSocialSpy(false);
            this._delayedSave();
        }

        return this.storage.getBoolean("social-spy", false);
    }

    protected final void _setSocialSpy(final boolean enabled) {
        this.storage.set("social-spy", enabled);
    }

    protected final @NotNull IChannel _getFocus() {
        final String name = this.storage.getString("focus");

        if (name != null && !name.isEmpty()) {
            final IChannel persist = this.instance.getChannelManager().getPersist(name);

            if (persist != null) {
                return persist;
            }
        }

        return this.instance.getChannelManager().getDefault();
    }

    protected final void _setFocus(@NotNull final IChannel focus) {
        this.storage.set("focus", focus.getFullName());
    }

    protected final @NotNull Set<IChannel> _getChannels() {
        if (!this.storage.contains("channels")) {
            this._setChannels(Collections.emptySet());
            this._delayedSave();
        }

        final Set<IChannel> channels = new HashSet<>();

        for (final String name : this.storage.getStringList("channels")) {
            if (name.isEmpty()) {
                continue;
            }

            final IChannel persist = this.instance.getChannelManager().getPersist(name);

            if (persist != null) {
                channels.add(persist);
            }
        }

        return channels;
    }

    protected final void _setChannels(@NotNull final Set<IChannel> channels) {
        this.storage.set("channels", channels.stream()
                .filter(IChannel::isPersist)
                .map(IChannel::getFullName)
                .collect(Collectors.toList()));
    }

    protected final @NotNull Set<UUID> _getIgnores() {
        if (!this.storage.contains("ignores")) {
            this._setIgnores(Collections.emptySet());
            this._delayedSave();
        }

        return new HashSet<>(this.storage.getUniqueIdList("ignores"));
    }

    protected final void _setIgnores(@NotNull final Set<UUID> ignores) {
        this.storage.set("ignores", ignores.stream().map(UUID::toString).collect(Collectors.toList()));
    }
}
