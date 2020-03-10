package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.storage.IStorageHolder;
import de.g4memas0n.Chats.util.logging.Log;
import de.g4memas0n.Chats.util.type.ChannelType;
import de.g4memas0n.Chats.messaging.IFormatter;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Representation of a persist channel that can be a default channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: March 9th, 2020
 */
public class PersistChannel extends StandardChannel implements IStorageHolder {

    private final IStorageFile storage;

    public PersistChannel(@NotNull final IFormatter formatter,
                          @NotNull final IStorageFile storage) {
        super(formatter, storage.getFile().getName().substring(0, storage.getFile().getName().lastIndexOf(".")));

        this.storage = storage;

        if (this.storage.getFile().exists()) {
            this.load();
        }
    }

    @Override
    public @NotNull IStorageFile getStorageFile() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();

            if (Log.isDebug()) {
                Log.getPluginLogger().info("Deleted channel file: " + this.storage.getFile().getName()
                        + " for channel: " + this.getFullName());
            }
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to delete channel file: " + this.storage.getFile().getName());
        }
    }

    @Override
    public void load() {
        try {
            this.storage.load();
        } catch (FileNotFoundException ex) {
            Log.getPluginLogger().warning("Unable to find channel file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Saving default settings for channel: " + this.getFullName());

            this.storage.clear();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPluginLogger().warning("Unable to load channel file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Using default settings for channel: " + this.getFullName());

            this.storage.clear();
        }

        super.setShortName(this._getShortName());

        try {
            super.setChatColor(this._getChatColor());
        } catch (IllegalArgumentException ex) {
            Log.getPluginLogger().warning("Invalid color in channel file: " + this.storage.getFile().getName()
                    + " reason: " + ex.getMessage());
        }

        super.setPassword(this._getPassword());
        super.setCrossWorld(this._getCrossWorld());
        super.setDistance(this._getDistance());

        for (final UUID current : this.getBanned()) {
            super.setBanned(current, false);
        }

        for (final UUID current : this._getBanned()) {
            super.setBanned(current, true);
        }

        for (final UUID current : this.getMuted()) {
            super.setMuted(current, false);
        }

        for (final UUID current : this._getMuted()) {
            super.setMuted(current, true);
        }

        for (final UUID current : this.getModerators()) {
            super.setModerator(current, false);
        }

        for (final UUID current : this._getModerators()) {
            super.setModerator(current, true);
        }

        try {
            super.setAnnounceFormat(this._getAnnounceFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPluginLogger().warning("Invalid announce format in channel file: "
                    + this.storage.getFile().getName() + " reason: " + ex.getMessage());
        }

        try {
            super.setBroadcastFormat(this._getBroadcastFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPluginLogger().warning("Invalid broadcast format in channel file: "
                    + this.storage.getFile().getName() + " reason: " + ex.getMessage());
        }

        try {
            super.setChatFormat(this._getChatFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPluginLogger().warning("Invalid chat format in channel file: "
                    + this.storage.getFile().getName() + " reason: " + ex.getMessage());
        }

        super.setCustomFormat(this._getCustomFormat());

        if (Log.isDebug()) {
            Log.getPluginLogger().info("Loaded channel file: " + this.storage.getFile().getName()
                    + " for channel: " + this.getFullName());
        }
    }

    @Override
    public void save() {
        try {
            this.storage.save();

            if (Log.isDebug()) {
                Log.getPluginLogger().info("Saved channel file: " + this.storage.getFile().getName()
                        + " for channel: " + this.getFullName());
            }
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to save channel file: " + this.storage.getFile().getName());
        }
    }

    // Channel Properties Methods:
    private @Nullable String _getShortName() {
        return this.storage.getString("short-name");
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) throws IllegalArgumentException {
        if (super.setShortName(shortName)) {
            this._setShortName(shortName);
            return true;
        }

        return false;
    }

    private void _setShortName(@Nullable final String shortName) {
        this.storage.set("short-name", shortName != null ? shortName : "");
    }

    private @Nullable ChatColor _getChatColor() {
        return this.storage.getChatColor("color");
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) {
        if (super.setChatColor(color)) {
            this._setChatColor(color);
            return true;
        }

        return false;
    }

    private void _setChatColor(@Nullable final ChatColor color) {
        this.storage.set("color", color != null ? color.name() : "");
    }

    private @Nullable String _getPassword() {
        return this.storage.getString("password");
    }

    @Override
    public boolean setPassword(@Nullable final String password) throws IllegalArgumentException {
        if (super.setPassword(password)) {
            this._setPassword(password);
            return true;
        }

        return false;
    }

    private void _setPassword(@Nullable final String password) {
        this.storage.set("password", password != null ? password : "");
    }

    private boolean _getCrossWorld() {
        return this.storage.getBoolean("cross-world", true);
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        if (super.setCrossWorld(enabled)) {
            this._setCrossWorld(enabled);
            return true;
        }

        return false;
    }

    private void _setCrossWorld(final boolean enabled) {
        this.storage.set("cross-world", enabled);
    }

    private int _getDistance() {
        return this.storage.getInt("distance", -1);
    }

    @Override
    public boolean setDistance(final int distance) {
        if (super.setDistance(distance)) {
            this._setDistance(distance);
            return true;
        }

        return false;
    }

    private void _setDistance(final int distance) {
        this.storage.set("distance", distance > 0 ? distance : -1);
    }

    @Override
    public boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final PersistChannel channel = (PersistChannel) object;
        return this.getFullName().equals(channel.getFullName());
    }

    @Override
    public int hashCode() {
        final int prime = 59;
        int result = 3;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public final @NotNull ChannelType getType() {
        return ChannelType.PERSIST;
    }

    public final boolean isConversation() {
        return false;
    }

    @Override
    public final boolean isPersist() {
        return true;
    }

    // Member, Banned, Muted, Moderator and Owner Methods:
    private @NotNull List<UUID> _getBanned() {
        return this.storage.getUniqueIdList("banned");
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        if (super.setBanned(uniqueId, banned)) {
            this._setBanned(this.getBanned());
            return true;
        }

        return false;
    }

    private void _setBanned(@NotNull final Set<UUID> banned) {
        this.storage.set("banned", banned.stream().map(UUID::toString).collect(Collectors.toSet()));
    }

    private @NotNull List<UUID> _getMuted() {
        return this.storage.getUniqueIdList("muted");
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        if (super.setMuted(uniqueId, muted)) {
            this._setMuted(this.getMuted());
            return true;
        }

        return false;
    }

    private void _setMuted(@NotNull final Set<UUID> muted) {
        this.storage.set("muted", muted.stream().map(UUID::toString).collect(Collectors.toSet()));
    }

    private @NotNull List<UUID> _getModerators() {
        return this.storage.getUniqueIdList("moderators");
    }

    @Override
    public boolean setModerator(@NotNull final UUID uniqueId, final boolean moderator) {
        if (super.setModerator(uniqueId, moderator)) {
            this._setModerators(this.getModerators());
            return true;
        }

        return false;
    }

    private void _setModerators(@NotNull final Set<UUID> moderators) {
        this.storage.set("moderators", moderators.stream().map(UUID::toString).collect(Collectors.toSet()));
    }

    @Override
    public @Nullable UUID getOwner() {
        return null;
    }

    @Override
    public boolean hasOwner() {
        return false;
    }

    @Override
    public boolean setOwner(@Nullable final UUID uniqueId) {
        return false;
    }

    @Override
    public boolean isOwner(@NotNull final UUID uniqueId) {
        return false;
    }

    // Formatting Methods:
    private @Nullable String _getAnnounceFormat() {
        return this.storage.getString("format.announce");
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setAnnounceFormat(format)) {
            this._setAnnounceFormat(format);
            return true;
        }

        return false;
    }

    private void _setAnnounceFormat(@Nullable final String format) {
        this.storage.set("format.announce", format != null ? format : "");
    }

    private @Nullable String _getBroadcastFormat() {
        return this.storage.getString("format.broadcast");
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setBroadcastFormat(format)) {
            this._setBroadcastFormat(format);
            return true;
        }

        return false;
    }

    private void _setBroadcastFormat(@Nullable final String format) {
        this.storage.set("format.broadcast", format != null ? format : "");
    }

    private @Nullable String _getChatFormat() {
        return this.storage.getString("format.chat");
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setChatFormat(format)) {
            this._setChatFormat(format);
            return true;
        }

        return false;
    }

    private void _setChatFormat(@Nullable final String format) {
        this.storage.set("format.chat", format != null ? format : "");
    }

    private boolean _getCustomFormat() {
        return this.storage.getBoolean("format.use-custom", false);
    }

    @Override
    public boolean setCustomFormat(final boolean enabled) {
        if (super.setCustomFormat(enabled)) {
            this._setCustomFormat(enabled);
            return true;
        }

        return false;
    }

    private void _setCustomFormat(final boolean enabled) {
        this.storage.set("format.use-custom", enabled);
    }
}
