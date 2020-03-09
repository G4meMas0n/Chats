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

/**
 * Representation of a persist channel that can be a default channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: March 8th, 2020
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
        super.setChatColor(this._getChatColor());
        super.setPassword(this._getPassword());
        super.setCrossWorld(this._getCrossWorld());
        super.setDistance(this._getDistance());
        super.setAnnounceFormat(this._getAnnounceFormat());
        super.setBroadcastFormat(this._getBroadcastFormat());
        super.setChatFormat(this._getChatFormat());
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
        final String shortName = this.storage.getString("short-name");

        if (shortName == null || shortName.isEmpty()) {
            return null;
        }

        return shortName;
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
        this.storage.set("color", color != null ? color.name() : null);
    }

    private @Nullable String _getPassword() {
        final String password = this.storage.getString("password");

        if (password == null || password.isEmpty()) {
            return null;
        }

        return password;
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
    public @NotNull ChannelType getType() {
        return ChannelType.PERSIST;
    }

    @Override
    public boolean isPersist() {
        return true;
    }

    // Channel Formatter and Performer Methods:
    private @Nullable String _getAnnounceFormat() {
        final String format = this.storage.getString("format.announce");

        if (format == null || format.isEmpty()) {
            return null;
        }

        return format;
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
        final String format = this.storage.getString("format.broadcast");

        if (format == null || format.isEmpty()) {
            return null;
        }

        return format;
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
        final String format = this.storage.getString("format.chat");

        if (format == null || format.isEmpty()) {
            return null;
        }

        return format;
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
