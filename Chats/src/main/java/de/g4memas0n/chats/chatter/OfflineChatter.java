package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.storage.YamlStorageFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.stream.Collectors;

/**
 * Implementation of a offline chatter.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class OfflineChatter extends StorageChatter implements IOfflineChatter {

    private final UUID uniqueId;

    private String name;

    protected OfflineChatter(@NotNull final Chats instance,
                             @NotNull final YamlStorageFile storage,
                             @NotNull final UUID uniqueId) {
        super(instance, storage);

        this.uniqueId = uniqueId;
    }

    @Override
    public synchronized void delete() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(true);
        }

        try {
            this.storage.delete();

            this.instance.getLogger().debug(String.format("Deleted storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to delete storage file '%s' of chatter with uuid '%s': %s",
                    this.storage.getFile().getName(), this.uniqueId.toString(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            this.instance.getLogger().debug(String.format("Loaded storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));
        } catch (MissingStorageFileException ex) {
            this.instance.getLogger().warning(String.format("Unable to find storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            this.instance.getLogger().log(Level.WARNING, String.format("Unable to load storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()), ex);

            this.storage.clear();
        }

        final UUID uniqueId = this._getUniqueId();

        if (!this.uniqueId.equals(uniqueId)) {
            this.instance.getLogger().warning(String.format("Detected %s unique-id in storage file '%s' of chatter with uuid: %s",
                    uniqueId != null ? "invalid" : "missing", this.storage.getFile().getName(), this.uniqueId.toString()));

            this._setUniqueId(this.uniqueId);
            this._delayedSave();
        }

        this.name = this._getLastName();

        if (this.name == null || this.name.isEmpty()) {
            this.instance.getLogger().warning(String.format("Detected missing name in storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));

            this.name = "Unknown";
        }
    }

    @Override
    public synchronized void save() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(false);
        }

        try {
            this.storage.save();

            this.instance.getLogger().debug(String.format("Saved storage file '%s' of chatter with uuid: %s",
                    this.storage.getFile().getName(), this.uniqueId.toString()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to save storage file '%s' of chatter with uuid '%s': %s",
                    this.storage.getFile().getName(), this.uniqueId.toString(), ex.getMessage()));
        }
    }

    @Override
    public final @NotNull String getName() {
        return this.name;
    }

    @Override
    public final @NotNull UUID getUniqueId() {
        return this.uniqueId;
    }

    @Override
    public final long getLastPlayed() {
        return this._getLastPlayed();
    }

    @Override
    public final boolean isMuted() {
        return this._getMuted();
    }

    @Override
    public final boolean setMuted(final boolean muted) {
        if (muted == this.isMuted()) {
            return false;
        }

        this._setMuted(muted);
        this._delayedSave();
        return true;
    }

    @Override
    public final boolean isSocialSpy() {
        return this._getSocialSpy();
    }

    @Override
    public final boolean setSocialSpy(final boolean enabled) {
        if (enabled == this.isSocialSpy()) {
            return false;
        }

        this._getSocialSpy();
        this._delayedSave();
        return true;
    }

    @Override
    public final @NotNull Set<UUID> getIgnores() {
        return this._getIgnores();
    }

    @Override
    public final boolean setIgnores(@NotNull final Set<UUID> ignores) {
        if (ignores.equals(this.getIgnores())) {
            return false;
        }

        this._setIgnores(ignores);
        this._delayedSave();
        return true;
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (object instanceof OfflineChatter) {
            final OfflineChatter other = (OfflineChatter) object;

            return this.uniqueId.equals(other.uniqueId);
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 59;
        int result = 3;

        result = prime * result + this.uniqueId.hashCode();

        return result;
    }

    @Override
    public final String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name=");
        builder.append(this.name);
        builder.append(";uniqueId=");
        builder.append(this.uniqueId.toString());
        builder.append(";muted=");
        builder.append(this.isMuted());

        final Set<UUID> ignores = this.getIgnores();

        if (!ignores.isEmpty()) {
            builder.append(";ignores=");
            builder.append(ignores.stream().map(UUID::toString).collect(Collectors.joining(",")));
        }

        return builder.append("}").toString();
    }
}
