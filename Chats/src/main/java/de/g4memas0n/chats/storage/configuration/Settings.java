package de.g4memas0n.chats.storage.configuration;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.IStorageFile;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.storage.YamlStorageFile;
import de.g4memas0n.chats.util.logging.Log;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.Locale;

/**
 * Implementation of the settings that represent the configuration file of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class Settings implements ISettings {

    private static final String FILE_CONFIG = "config";

    private final IStorageFile storage;
    private final IChats instance;

    private String formatAnnounce;
    private String formatBroadcast;
    private String formatChat;
    private String formatConversation;
    private String defaultChannel;

    private boolean update;

    public Settings(@NotNull final IChats instance) {
        this.storage = new YamlStorageFile(instance.getDataFolder(), FILE_CONFIG);
        this.instance = instance;
        this.update = false;

        this.load();
    }

    @Override
    public @NotNull IStorageFile getStorage() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();

            Log.getPlugin().debug("Deleted config file: " + this.storage.getFile().getName());
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to delete config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public void load() {
        this.update = false;

        try {
            this.storage.load();

            Log.getPlugin().debug("Loaded config file: " + this.storage.getFile().getName());
        } catch (MissingStorageFileException ex) {
            Log.getPlugin().warning(String.format("Unable to find config file '%s'. Saving default configuration...",
                    this.storage.getFile().getName()));

            this.instance.saveResource(FILE_CONFIG + ".yml", true);
            this.load();
        } catch (InvalidStorageFileException ex) {
            Log.getPlugin().severe(String.format("Unable to load config file '%s', because it is broken. Renaming it "
                    + "and saving default configuration...", this.storage.getFile().getName()));

            final File broken = new File(this.instance.getDataFolder(), FILE_CONFIG + ".broken.yml");

            if (broken.exists() && broken.delete()) {
                Log.getPlugin().debug("Deleted old broken config file: " + broken.getName());
            }

            if (this.storage.getFile().renameTo(broken)) {
                Log.getPlugin().debug("Renamed broken config file to: " + broken.getName());
            }

            this.instance.saveResource(FILE_CONFIG + ".yml", true);
            this.load();
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to load config file '%s'. Loading default configuration...",
                    this.storage.getFile().getName()));

            this.storage.clear();
        }

        try {
            this.setAnnounceFormat(this._getAnnounceFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPlugin().warning(String.format("Detected invalid announce-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));

            this.formatAnnounce = "{color}{message}";
        }

        try {
            this.setBroadcastFormat(this._getBroadcastFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPlugin().warning(String.format("Detected invalid broadcast-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));

            this.formatBroadcast = "{color}[{nick}][§aBroadcast§r{color}] {message}";
        }

        try {
            this.setChatFormat(this._getChatFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPlugin().warning(String.format("Detected invalid chat-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));

            this.formatChat = "{color}[{nick}]§r{sender}{color}: {message}";
        }

        try {
            this.setConversationFormat(this._getConversationFormat());
        } catch (IllegalArgumentException ex) {
            Log.getPlugin().warning(String.format("Detected invalid conversation-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));

            this.formatConversation = "{color}{con-address} §r{con-partner}{color}: {message}";
        }

        this.setDefaultChannel(this._getDefaultChannel());

        this._getLocale();
        this._getLogColored();
        this._getLogDebug();
        this._getLogToConsole();
        this._getLogToFile();
        this._getAutoSave();
        this._getSaveDelay();

        if (this.update) {
            this.save();
        }
    }

    @Override
    public void save() {
        try {
            this.storage.save();

            Log.getPlugin().debug("Saved config file: " + this.storage.getFile().getName());
        } catch (IOException ex) {
            Log.getPlugin().warning(String.format("Unable to save config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    // Format Settings:
    protected @NotNull String _getAnnounceFormat() {
        final String format = this.storage.getString("format.announce");

        if (format == null || format.isEmpty()) {
            final String def = "{color}{message}";

            this.storage.set("format.announce", def);
            this.update = true;

            return def;
        }

        return format;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        return this.formatAnnounce;
    }

    public void setAnnounceFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatAnnounce)) {
            return;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        this.formatAnnounce = format;
    }

    protected @NotNull String _getBroadcastFormat() {
        final String format = this.storage.getString("format.broadcast");

        if (format == null || format.isEmpty()) {
            final String def = "{color}[{nick}][§aBroadcast§r{color}] {message}";

            this.storage.set("format.broadcast", def);
            this.update = true;

            return def;
        }

        return format;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        return this.formatBroadcast;
    }

    public void setBroadcastFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatBroadcast)) {
            return;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        this.formatBroadcast = format;
    }

    protected @NotNull String _getChatFormat() {
        final String format = this.storage.getString("format.chat");

        if (format == null || format.isEmpty()) {
            final String def = "{color}[{nick}]§r{sender}{color}: {message}";

            this.storage.set("format.chat", def);
            this.update = true;

            return def;
        }

        return format;
    }

    @Override
    public @NotNull String getChatFormat() {
        return this.formatChat;
    }

    public void setChatFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatChat)) {
            return;
        }

        if (!format.contains(Placeholder.SENDER.toString()) && !format.contains(Placeholder.SENDER_PLAIN.toString())) {
            throw new IllegalArgumentException("Format is missing {sender} or {sender-plain} placeholder: " + format);
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        this.formatChat = format;
    }

    protected @NotNull String _getConversationFormat() {
        final String format = this.storage.getString("format.conversation");

        if (format == null || format.isEmpty()) {
            final String def = "{color}{con-address} §r{con-partner}{color}: {message}";

            this.storage.set("format.conversation", def);
            this.update = true;

            return def;
        }

        return format;
    }

    @Override
    public @NotNull String getConversationFormat() {
        return this.formatConversation;
    }

    public void setConversationFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatConversation)) {
            return;
        }

        if (!format.contains(Placeholder.CON_ADDRESS.toString())) {
            throw new IllegalArgumentException("Format is missing {con-address} placeholder: " + format);
        }

        if (!format.contains(Placeholder.CON_PARTNER.toString())) {
            throw new IllegalArgumentException("Format is missing {con-partner} placeholder: " + format);
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException("Format is missing {message} placeholder: " + format);
        }

        this.formatConversation = format;
    }

    // Default Channel Setting:
    protected @NotNull String _getDefaultChannel() {
        final String fullName = this.storage.getString("default-channel");

        if (fullName == null || fullName.isEmpty()) {
            final String def = "Global";

            this.storage.set("default-channel", def);
            this.update = true;

            return def;
        }

        return fullName;
    }

    @Override
    public @NotNull String getDefaultChannel() {
        return this.defaultChannel;
    }

    public void setDefaultChannel(@NotNull final String fullName) {
        if (fullName.equals(this.defaultChannel)) {
            return;
        }

        this.defaultChannel = fullName;
    }

    // Locale Setting:
    protected @NotNull Locale _getLocale() {
        final Locale locale = this.storage.getLocale("locale");

        if (locale == null) {
            final Locale def = Locale.ENGLISH;

            this.storage.set("locale", def.toString());
            this.update = true;

            return Locale.ENGLISH;
        }

        return locale;
    }

    @Override
    public @NotNull Locale getLocale() {
        return this._getLocale();
    }

    // Log Settings:

    protected boolean _getLogColored() {
        if (!this.storage.contains("log.chat.colored")) {
            this.storage.set("log.chat.colored", true);
            this.update = true;
        }

        return this.storage.getBoolean("log.chat.colored", true);
    }

    @Override
    public boolean isLogColored() {
        return this._getLogColored();
    }

    private boolean _getLogDebug() {
        if (!this.storage.contains("log.debug")) {
            this.storage.set("log.debug", false);
            this.update = true;
        }

        return this.storage.getBoolean("log.debug", false);
    }

    @Override
    public boolean isLogDebug() {
        return this._getLogDebug();
    }

    protected boolean _getLogToConsole() {
        if (!this.storage.contains("log.chat.to-console")) {
            this.storage.set("log.chat.to-console", true);
            this.update = true;
        }

        return this.storage.getBoolean("log.chat.to-console", true);
    }

    @Override
    public boolean isLogToConsole() {
        return this._getLogToConsole();
    }

    protected boolean _getLogToFile() {
        if (!this.storage.contains("log.chat.to-file")) {
            this.storage.set("log.chat.to-file", true);
            this.update = true;
        }

        return this.storage.getBoolean("log.chat.to-file", true);
    }

    @Override
    public boolean isLogToFile() {
        return this._getLogToFile();
    }

    protected boolean _getAutoSave() {
        if (!this.storage.contains("auto-save.enabled")) {
            this.storage.set("auto-save.enabled", true);
            this.update = true;
        }

        return this.storage.getBoolean("auto-save.enabled", true);
    }

    @Override
    public boolean isAutoSave() {
        return this._getAutoSave();
    }

    protected int _getSaveDelay() {
        if (!this.storage.contains("auto-save.delay")) {
            this.storage.set("auto-save.delay", 60);
            this.update = true;
        }

        return this.storage.getInt("auto-save.delay", 60);
    }

    @Override
    public int getSaveDelay() {
        return this._getSaveDelay();
    }
}
