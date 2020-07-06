package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * Implementation of a offline chatter.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class OfflineChatter implements IOfflineChatter, Comparable<IOfflineChatter> {

    private final IStorageFile storage;
    private final IChats instance;
    private final UUID uniqueId;

    private Future<?> saveTask;

    protected OfflineChatter(@NotNull final IChats instance,
                             @NotNull final IStorageFile storage,
                             @NotNull final UUID uniqueId) {
        this.storage = storage;
        this.instance = instance;
        this.uniqueId = uniqueId;
    }

    @Override
    public @NotNull IStorageFile getStorage() {
        return this.storage;
    }

    @Override
    public synchronized void delete() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(true);
        }

        try {
            this.storage.delete();

            Log.getPlugin().debug(String.format("Deleted storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to delete storage file '%s' of chatter with uuid '%s': %s",
                    this.storage.getFile().getName(), this.uniqueId.toString(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            Log.getPlugin().debug(String.format("Loaded chatter with uuid '%s' from storage file: %s",
                    this.uniqueId.toString(), this.storage.getFile().getName()));
        } catch (MissingStorageFileException ex) {
            Log.getPlugin().warning(String.format("Unable to find storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPlugin().warning(String.format("Unable to load storage file '%s' of chatter with uuid '%s': %s",
                    this.storage.getFile().getName(), this.uniqueId.toString(), ex.getMessage()));

            this.storage.clear();
        }

        if (!this.uniqueId.equals(this.storage.getUniqueId("uuid"))) {
            Log.getPlugin().warning(String.format("Detected %s unique-id in storage file '%s' of chatter with uuid: %s",
                    this.storage.contains("uuid") ? "invalid" : "missing", this.storage.getFile().getName(), this.uniqueId.toString()));

            this.storage.set("uuid", this.uniqueId.toString());
            this._delayedSave();
        }
    }

    @Override
    public synchronized void save() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(false);
        }

        try {
            this.storage.save();

            Log.getPlugin().debug(String.format("Saved chatter with uuid '%s' to storage file: %s",
                    this.uniqueId.toString(), this.storage.getFile().getName()));
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to save storage file '%s' of chatter with uuid '%s': %s",
                    this.storage.getFile().getName(), this.uniqueId.toString(), ex.getMessage()));
        }
    }

    protected synchronized final void _delayedSave() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            return;
        }

        this.saveTask = this.instance.scheduleStorageTask(this::save);
    }

    @Override
    public final @NotNull UUID getUniqueId() {
        return this.uniqueId;
    }

    @Override
    public final @NotNull String getName() {
        final String name = this.storage.getString("last.name");

        if (name == null || name.isEmpty()) {
            return "Unknown";
        }

        return name;
    }

    @Override
    public final long getLastPlayed() {
        return this.storage.getLong("last.played", -1);
    }

    @Override
    public final boolean isMuted() {
        return this.storage.getBoolean("muted", false);
    }

    @Override
    public final boolean setMuted(final boolean muted) {
        if (muted == this.isMuted()) {
            return false;
        }

        this.storage.set("muted", muted);
        this._delayedSave();
        return true;
    }

    @Override
    public final boolean isSocialSpy() {
        return this.storage.getBoolean("social-spy", false);
    }

    public final boolean setSocialSpy(final boolean enabled) {
        if (enabled == this.isSocialSpy()) {
            return false;
        }

        this.storage.set("social-spy", enabled);
        this._delayedSave();
        return true;
    }

    @Override
    public final @NotNull Set<UUID> getIgnores() {
        return new HashSet<>(this.storage.getUniqueIdList("ignores"));
    }

    @Override
    public final boolean setIgnores(@NotNull final Set<UUID> ignores) {
        if (ignores.equals(this.getIgnores())) {
            return false;
        }

        this.storage.set("ignores", ignores.stream().map(UUID::toString).collect(Collectors.toList()));
        this._delayedSave();
        return true;
    }

    @Override
    public final String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name=");
        builder.append(this.getName());
        builder.append(";uniqueId=");
        builder.append(this.getUniqueId());
        builder.append(";muted=");
        builder.append(this.isMuted());

        final Set<UUID> ignores = this.getIgnores();

        if (!ignores.isEmpty()) {
            builder.append(";ignores=");
            builder.append(ignores.stream().map(UUID::toString).collect(Collectors.joining(",")));
        }

        return builder.append("}").toString();
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (object instanceof IOfflineChatter) {
            final IOfflineChatter other = (IChatter) object;

            return this.getUniqueId().equals(other.getUniqueId());
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 59;
        int result = 3;

        result = prime * result + this.getUniqueId().hashCode();

        return result;
    }

    @Override
    public final int compareTo(@NotNull final IOfflineChatter other) {
        return this.getName().compareTo(other.getName());
    }
}
