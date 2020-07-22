package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.IStorageHolder;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.util.type.ChannelType;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.stream.Collectors;

/**
 * Implementation of a persist channel that is persist over server restarts and can be used as default channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public class PersistChannel extends StandardChannel implements IStorageHolder {

    private final IStorageFile storage;

    private Future<?> saveTask;

    public PersistChannel(@NotNull final Chats instance,
                          @NotNull final IStorageFile storage) throws IllegalArgumentException {
        super(instance, storage.getFile().getName().substring(0, storage.getFile().getName().lastIndexOf(".")));

        this.storage = storage;
    }

    @Override
    public synchronized void delete() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(true);
        }

        try {
            this.storage.delete();

            this.instance.getLogger().debug(String.format("Deleted storage file '%s' of channel: %s",
                    this.storage.getFile().getName(), this.getFullName()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to delete storage file '%s' of channel '%s': %s",
                    this.storage.getFile().getName(), this.getFullName(), ex.getMessage()));
        }
    }

    @Override
    public synchronized void load() {
        try {
            this.storage.load();

            this.instance.getLogger().debug(String.format("Loaded storage file '%s' of channel: %s",
                    this.storage.getFile().getName(), this.getFullName()));
        } catch (MissingStorageFileException ex) {
            this.instance.getLogger().warning(String.format("Unable to find storage file '%s' of channel '%s'. Loading "
                            + "default configuration...", this.storage.getFile().getName(), this.getFullName()));

            this.storage.clear();
            this._delayedSave();
        } catch (IOException | InvalidStorageFileException ex) {
            this.instance.getLogger().log(Level.WARNING, String.format("Unable to load storage file '%s' of channel '%s'. " +
                    "Loading previous configuration...", this.storage.getFile().getName(), this.getFullName()), ex);
        }

        final String fullName = this._getFullName();

        if (!this.getFullName().equals(fullName)) {
            this.instance.getLogger().warning(String.format("Detected %s full-name in storage file '%s' of channel: %s",
                    fullName != null ? "invalid" : "missing", this.storage.getFile().getName(), this.getFullName()));

            this._setFullName(this.getFullName());
            this._delayedSave();
        }

        super.setShortName(this._getShortName());

        try {
            super.setColor(this._getColor());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid color in storage file '%s' of channel '%s': %s",
                    this.storage.getFile().getName(), this.getFullName(), ex.getMessage()));
        }

        super.setPassword(this._getPassword());
        super.setDistance(this._getDistance());
        super.setCrossWorld(this._getCrossWorld());
        super.setVerbose(this._getVerbose());
        super.setCustomFormat(this._getCustomFormat());

        try {
            super.setAnnounceFormat(this._getAnnounceFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid announce-format in storage file '%s' of channel '%s': %s",
                    this.storage.getFile().getName(), this.getFullName(), ex.getMessage()));
        }

        try {
            super.setBroadcastFormat(this._getBroadcastFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid broadcast-format in storage file '%s' of channel '%s': %s",
                    this.storage.getFile().getName(), this.getFullName(), ex.getMessage()));
        }

        try {
            super.setChatFormat(this._getChatFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid chat-format in storage file '%s' of channel '%s': %s",
                    this.storage.getFile().getName(), this.getFullName(), ex.getMessage()));
        }

        super.setBans(this._getBans());
        super.setMutes(this._getMutes());
    }

    @Override
    public synchronized void save() {
        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            this.saveTask.cancel(false);
        }

        if (!this.storage.getFile().exists()) {
            this._setFullName(this.getFullName());
            this._setShortName(this.getShortName());
            this._setColor(this.getColor());
            this._setPassword(this.getPassword());
            this._setDistance(this.getDistance());
            this._setCrossWorld(this.isCrossWorld());
            this._setVerbose(this.isVerbose());
            this._setCustomFormat(this.isCustomFormat());
            this._setAnnounceFormat(this.getAnnounceFormat());
            this._setBroadcastFormat(this.getBroadcastFormat());
            this._setChatFormat(this.getChatFormat());
            this._setBans(this.getBans());
            this._setMutes(this.getMutes());
        }

        try {
            this.storage.save();

            this.instance.getLogger().debug(String.format("Saved storage file '%s' of channel: %s",
                    this.storage.getFile().getName(), this.getFullName()));
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to save channel '%s' to storage file '%s': %s",
                    this.getFullName(), this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    protected synchronized final void _delayedSave() {
        if (!this.instance.getSettings().isAutoSave()) {
            return;
        }

        if (this.saveTask != null && !this.saveTask.isDone() && !this.saveTask.isCancelled()) {
            return;
        }

        this.saveTask = this.instance.scheduleStorageTask(this::save);
    }

    protected @Nullable String _getFullName() {
        return this.storage.getString("full-name");
    }

    protected void _setFullName(@NotNull final String fullName) {
        this.storage.set("full-name", fullName);
    }

    // Channel Properties Methods:
    protected @Nullable String _getShortName() {
        if (!this.storage.contains("short-name")) {
            this._setShortName(null);
            this._delayedSave();
        }

        return this.storage.getString("short-name");
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        if (super.setShortName(shortName)) {
            this._setShortName(shortName);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setShortName(@Nullable final String shortName) {
        this.storage.set("short-name", (shortName == null || shortName.equals(this.getFullName())) ? "" : shortName);
    }

    protected @Nullable ChatColor _getColor() {
        if (!this.storage.contains("color")) {
            this._setColor(null);
            this._delayedSave();
        }

        return this.storage.getChatColor("color");
    }

    @Override
    public boolean setColor(@Nullable final ChatColor color) {
        if (super.setColor(color)) {
            this._setColor(color);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setColor(@Nullable final ChatColor color) {
        this.storage.set("color", color != null ? color.name() : ChatColor.WHITE.name());
    }

    protected @Nullable String _getPassword() {
        if (!this.storage.contains("password")) {
            this._setPassword(null);
            this._delayedSave();
        }

        return this.storage.getString("password");
    }

    @Override
    public boolean setPassword(@Nullable final String password) throws IllegalArgumentException {
        if (super.setPassword(password)) {
            this._setPassword(password);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setPassword(@Nullable final String password) {
        this.storage.set("password", password != null ? password : "");
    }

    protected int _getDistance() {
        if (!this.storage.contains("distance")) {
            this._setDistance(-1);
            this._delayedSave();
        }

        return this.storage.getInt("distance", -1);
    }

    @Override
    public boolean setDistance(final int distance) {
        if (super.setDistance(distance)) {
            this._setDistance(distance);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setDistance(final int distance) {
        this.storage.set("distance", distance > 0 ? distance : -1);
    }

    protected boolean _getCrossWorld() {
        if (!this.storage.contains("cross-world")) {
            this._setCrossWorld(true);
            this._delayedSave();
        }

        return this.storage.getBoolean("cross-world", true);
    }

    @Override
    public boolean setCrossWorld(final boolean crossWorld) {
        if (super.setCrossWorld(crossWorld)) {
            this._setCrossWorld(crossWorld);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setCrossWorld(final boolean crossWorld) {
        this.storage.set("cross-world", crossWorld);
    }

    protected boolean _getVerbose() {
        if (!this.storage.contains("verbose")) {
            this._setVerbose(true);
            this._delayedSave();
        }

        return this.storage.getBoolean("verbose", true);
    }

    @Override
    public boolean setVerbose(final boolean verbose) {
        if (super.setVerbose(verbose)) {
            this._setVerbose(verbose);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setVerbose(final boolean verbose) {
        this.storage.set("verbose", verbose);
    }

    protected boolean _getCustomFormat() {
        if (!this.storage.contains("format.use-custom")) {
            this._setCustomFormat(false);
            this._delayedSave();
        }

        return this.storage.getBoolean("format.use-custom", false);
    }

    @Override
    public boolean setCustomFormat(final boolean enabled) {
        if (super.setCustomFormat(enabled)) {
            this._setCustomFormat(enabled);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setCustomFormat(final boolean enabled) {
        this.storage.set("format.use-custom", enabled);
    }

    protected @Nullable String _getAnnounceFormat() {
        if (!this.storage.contains("format.announce")) {
            this._setAnnounceFormat(null);
            this._delayedSave();
        }

        return this.storage.getString("format.announce");
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setAnnounceFormat(format)) {
            this._setAnnounceFormat(format);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setAnnounceFormat(@Nullable final String format) {
        this.storage.set("format.announce", (format == null || format.equals(this.instance.getSettings().getAnnounceFormat())) ? "" : format);
    }

    protected @Nullable String _getBroadcastFormat() {
        if (!this.storage.contains("format.broadcast")) {
            this._setBroadcastFormat(null);
            this._delayedSave();
        }

        return this.storage.getString("format.broadcast");
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setBroadcastFormat(format)) {
            this._setBroadcastFormat(format);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setBroadcastFormat(@Nullable final String format) {
        this.storage.set("format.broadcast", (format == null || format.equals(this.instance.getSettings().getBroadcastFormat())) ? "" : format);
    }

    protected @Nullable String _getChatFormat() {
        if (!this.storage.contains("format.chat")) {
            this._setChatFormat(null);
            this._delayedSave();
        }

        return this.storage.getString("format.chat");
    }

    @Override
    public boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException {
        if (super.setChatFormat(format)) {
            this._setChatFormat(format);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setChatFormat(@Nullable final String format) {
        this.storage.set("format.chat", (format == null || format.equals(this.instance.getSettings().getChatFormat())) ? "" : format);
    }

    // Channel Type Methods:
    @Override
    public final @NotNull ChannelType getType() {
        return ChannelType.PERSIST;
    }

    // Channel Collection Methods:
    protected @NotNull Set<UUID> _getBans() {
        if (!this.storage.contains("bans")) {
            this._setBans(Collections.emptySet());
            this._delayedSave();
        }

        return new HashSet<>(this.storage.getUniqueIdList("bans"));
    }

    @Override
    public boolean setBans(@NotNull final Set<UUID> bans) {
        if (super.setBans(bans)) {
            this._setBans(bans);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setBans(@NotNull final Set<UUID> bans) {
        this.storage.set("bans", bans.stream().map(UUID::toString).collect(Collectors.toList()));
    }

    @Override
    public boolean setBanned(@NotNull final UUID uniqueId, final boolean banned) {
        if (super.setBanned(uniqueId, banned)) {
            this._setBans(this.getBans());
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected @NotNull Set<UUID> _getMutes() {
        if (!this.storage.contains("mutes")) {
            this._setMutes(Collections.emptySet());
            this._delayedSave();
        }

        return new HashSet<>(this.storage.getUniqueIdList("mutes"));
    }

    @Override
    public boolean setMutes(@NotNull final Set<UUID> mutes) {
        if (super.setMutes(mutes)) {
            this._setMutes(mutes);
            this._delayedSave();
            return true;
        }

        return false;
    }

    protected void _setMutes(@NotNull final Set<UUID> mutes) {
        this.storage.set("mutes", mutes.stream().map(UUID::toString).collect(Collectors.toList()));
    }

    @Override
    public boolean setMuted(@NotNull final UUID uniqueId, final boolean muted) {
        if (super.setMuted(uniqueId, muted)) {
            this._setMutes(this.getMutes());
            this._delayedSave();
            return true;
        }

        return false;
    }

    @Override
    public boolean setOwner(@Nullable final UUID uniqueId) {
        return false;
    }
}
