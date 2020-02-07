package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.channel.type.ChannelType;
import de.g4memas0n.Chats.chat.IChatFormatter;
import de.g4memas0n.Chats.chat.IChatPerformer;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.IOException;

/**
 * Representation of a persist channel that can be a default channel, extends the {@link StandardChannel} class.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 3rd, 2020
 * changed: February 3rd, 2020
 */
public final class PersistChannel extends StandardChannel {

    /**
     * the yaml storage paths of all saved channel options.
     */
    private static final String PATH_SHORT_NAME = "short-name";
    private static final String PATH_COLOR = "color";
    private static final String PATH_CROSS_WORLD = "cross-world";
    private static final String PATH_DISTANCE = "distance";
    private static final String PATH_PASSWORD = "password";
    private static final String PATH_FORMAT_ANNOUNCE = "format.announce";
    private static final String PATH_FORMAT_BROADCAST = "format.broadcast";
    private static final String PATH_FORMAT_CHANNEL = "format.channel";
    private static final String PATH_FORMAT_USE_CUSTOM = "format.use-custom";

    private final IStorageFile storage;

    public PersistChannel(@NotNull final IChatFormatter formatter,
                          @NotNull final IChatPerformer performer,
                          @NotNull final File file) throws IllegalArgumentException {
        super(formatter, performer, file.getName().substring(0, file.getName().lastIndexOf(".")));

        this.storage = new YamlStorageFile(file);
    }

    @Override
    public void delete() {
        this.storage.delete();
    }

    @Override
    public void reload() {
        try {
            this.storage.load();
        } catch (IOException | InvalidStorageFileException ex) {
            //TODO: Log reload failure.
            return;
        }

        super.setShortName(this.storage.getString(PATH_SHORT_NAME));
        super.setChatColor(this.storage.getChatColor(PATH_COLOR));
        super.setPassword(this.storage.getString(PATH_PASSWORD));
        super.setCrossWorld(this.storage.getBoolean(PATH_CROSS_WORLD, true));
        super.setDistance(this.storage.getInt(PATH_DISTANCE, -1));
        super.setAnnounceFormat(this.storage.getString(PATH_FORMAT_ANNOUNCE));
        super.setBroadcastFormat(this.storage.getString(PATH_FORMAT_BROADCAST));
        super.setChatFormat(this.storage.getString(PATH_FORMAT_CHANNEL));
        super.setUseCustomFormat(this.storage.getBoolean(PATH_FORMAT_USE_CUSTOM, false));

        //TODO: Log load success.
    }

    @Override
    public void save() {
        this.storage.set(PATH_SHORT_NAME, this.getShortName());
        this.storage.set(PATH_COLOR, this.getChatColor().name());
        this.storage.set(PATH_PASSWORD, this.getPassword());
        this.storage.set(PATH_CROSS_WORLD, this.isCrossWorld());
        this.storage.set(PATH_DISTANCE, this.getDistance());

        try {
            this.storage.save();
            //TODO: Log save success.
        } catch (IOException ex) {
            //TODO: Log save failure.
        }
    }

    // Channel Properties Methods:
    @Override
    public boolean setShortName(@Nullable final String shortName) throws IllegalArgumentException {
        if (super.setShortName(shortName)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setChatColor(@Nullable final ChatColor color) {
        if (super.setChatColor(color)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        if (super.setPassword(password)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        if (super.setCrossWorld(enabled)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setDistance(final int distance) {
        if (super.setDistance(distance)) {
            this.save();
            return true;
        }

        return false;
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
        final int prime = 41;
        int result = 3;

        result = prime * result + this.getFullName().hashCode();

        return result;
    }

    // Channel Type Methods:
    @Override
    public @NotNull ChannelType getTpe() {
        return ChannelType.PERSIST;
    }

    @Override
    public boolean isPersist() {
        return true;
    }

    // Channel Formatter and Performer Methods:
    @Override
    public boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setAnnounceFormat(format)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setBroadcastFormat(format)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setChatFormat(format)) {
            this.save();
            return true;
        }

        return false;
    }

    @Override
    public boolean setUseCustomFormat(final boolean enabled) {
        if (super.setUseCustomFormat(enabled)) {
            this.save();
            return true;
        }

        return false;
    }
}
