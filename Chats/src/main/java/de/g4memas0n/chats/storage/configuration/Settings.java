package de.g4memas0n.chats.storage.configuration;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.storage.InvalidStorageFileException;
import de.g4memas0n.chats.storage.MissingStorageFileException;
import de.g4memas0n.chats.storage.YamlStorageFile;
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

    private final Chats instance;
    private final YamlStorageFile storage;

    private String formatAnnounce;
    private String formatBroadcast;
    private String formatChat;
    private String formatConversation;
    private String defaultChannel;

    private boolean inform;
    private boolean save;

    private long informDelay;
    private long saveDelay;

    private int createLimit;

    public Settings(@NotNull final Chats instance) {
        this.instance = instance;
        this.storage = new YamlStorageFile(instance.getDataFolder(), FILE_CONFIG);
    }

    public @NotNull YamlStorageFile getStorage() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();

            this.instance.getLogger().debug("Deleted config file: " + this.storage.getFile().getName());
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to delete config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
    }

    @Override
    public void load() {
        try {
            this.storage.load();

            this.instance.getLogger().debug("Loaded config file: " + this.storage.getFile().getName());
        } catch (MissingStorageFileException ex) {
            this.instance.getLogger().warning(String.format("Unable to find config file '%s'. "
                    + "Saving default configuration...", this.storage.getFile().getName()));

            this.instance.saveResource(FILE_CONFIG + ".yml", true);
            this.instance.getLogger().info(String.format("Saved default configuration from template: %s.yml", FILE_CONFIG));
            this.load();
        } catch (InvalidStorageFileException ex) {
            this.instance.getLogger().severe(String.format("Unable to load config file '%s', because it is broken. "
                    + "Renaming it and saving default configuration...", this.storage.getFile().getName()));

            final File broken = new File(this.instance.getDataFolder(), FILE_CONFIG + ".broken.yml");

            if (broken.exists() && broken.delete()) {
                this.instance.getLogger().debug("Deleted old broken config file: " + broken.getName());
            }

            if (this.storage.getFile().renameTo(broken)) {
                this.instance.getLogger().info(String.format("Renamed broken config file '%s' to: %s",
                        this.storage.getFile().getName(), broken.getName()));
            }

            this.instance.saveResource(FILE_CONFIG + ".yml", true);
            this.instance.getLogger().info(String.format("Saved default configuration from template: %s.yml", FILE_CONFIG));
            this.load();
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to load config file '%s'. "
                    + "Loading default configuration...", this.storage.getFile().getName()));

            this.storage.clear();
        }

        try {
            this.setAnnounceFormat(this._getAnnounceFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid announce-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }

        try {
            this.setBroadcastFormat(this._getBroadcastFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid broadcast-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }

        try {
            this.setChatFormat(this._getChatFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid chat-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }

        try {
            this.setConversationFormat(this._getConversationFormat());
        } catch (IllegalArgumentException ex) {
            this.instance.getLogger().warning(String.format("Detected invalid conversation-format in config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }

        this.defaultChannel = this._getDefaultChannel();

        this.inform = this._getInform();
        this.save = this._getAutoSave();

        this.informDelay = this._getInformDelay();
        this.saveDelay = this._getSaveDelay();

        this.createLimit = this._getCreateLimit();
    }

    @Override
    public void save() {
        /*
        Disabled, because it is not intended to save the config file, as this breaks the comments.

        try {
            this.storage.save();

            this.instance.getLogger().debug("Saved config file: " + this.storage.getFile().getName());
        } catch (IOException ex) {
            this.instance.getLogger().warning(String.format("Unable to save config file '%s': %s",
                    this.storage.getFile().getName(), ex.getMessage()));
        }
         */
    }

    // Format Settings:
    protected @NotNull String _getAnnounceFormat() {
        final String format = this.storage.getString("format.announce");

        if (format == null || format.isEmpty()) {
            return ANNOUNCE_FORMAT;
        }

        return format;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        if (this.formatAnnounce == null) {
            return ANNOUNCE_FORMAT;
        }

        return this.formatAnnounce;
    }

    public void setAnnounceFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatAnnounce)) {
            return;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.formatAnnounce = format;
    }

    protected @NotNull String _getBroadcastFormat() {
        final String format = this.storage.getString("format.broadcast");

        if (format == null || format.isEmpty()) {
            return BROADCAST_FORMAT;
        }

        return format;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        if (this.formatBroadcast == null) {
            return BROADCAST_FORMAT;
        }

        return this.formatBroadcast;
    }

    public void setBroadcastFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatBroadcast)) {
            return;
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.formatBroadcast = format;
    }

    protected @NotNull String _getChatFormat() {
        final String format = this.storage.getString("format.chat");

        if (format == null || format.isEmpty()) {
            return CHAT_FORMAT;
        }

        return format;
    }

    @Override
    public @NotNull String getChatFormat() {
        if (this.formatChat == null) {
            return CHAT_FORMAT;
        }

        return this.formatChat;
    }

    public void setChatFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatChat)) {
            return;
        }

        if (!format.contains(Placeholder.SENDER.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {sender} placeholder", format));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.formatChat = format;
    }

    protected @NotNull String _getConversationFormat() {
        final String format = this.storage.getString("format.conversation");

        if (format == null || format.isEmpty()) {
            return CONVERSATION_FORMAT;
        }

        return format;
    }

    @Override
    public @NotNull String getConversationFormat() {
        if (this.formatConversation == null) {
            return CONVERSATION_FORMAT;
        }

        return this.formatConversation;
    }

    public void setConversationFormat(@NotNull final String format) throws IllegalArgumentException {
        if (format.equals(this.formatConversation)) {
            return;
        }

        if (!format.contains(Placeholder.CON_ADDRESS.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {con-address} placeholder", format));
        }

        if (!format.contains(Placeholder.CON_PARTNER.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {con-partner} placeholder", format));
        }

        if (!format.contains(Placeholder.MESSAGE.toString())) {
            throw new IllegalArgumentException(String.format("Format '%s' is missing {message} placeholder", format));
        }

        this.formatConversation = format;
    }

    // Default Channel Setting:
    protected @NotNull String _getDefaultChannel() {
        final String fullName = this.storage.getString("default-channel");

        if (fullName == null || fullName.isEmpty()) {
            return DEFAULT_CHANNEL;
        }

        return fullName;
    }

    @Override
    public @NotNull String getDefaultChannel() {
        if (this.defaultChannel == null) {
            return this._getDefaultChannel();
        }

        return this.defaultChannel;
    }

    // Locale Setting:
    protected @NotNull Locale _getLocale() {
        final Locale locale = this.storage.getLocale("locale");

        if (locale == null) {
            return LOCALE;
        }

        return locale;
    }

    @Override
    public @NotNull Locale getLocale() {
        return this._getLocale();
    }

    // Log Settings:
    protected boolean _getLogColored() {
        return this.storage.getBoolean("log.chat.colored", LOG_COLORED);
    }

    @Override
    public boolean isLogColored() {
        return this._getLogColored();
    }

    private boolean _getLogDebug() {
        return this.storage.getBoolean("log.debug", LOG_DEBUG);
    }

    @Override
    public boolean isLogDebug() {
        return this._getLogDebug();
    }

    protected boolean _getLogToConsole() {
        return this.storage.getBoolean("log.chat.to-console", LOG_TO_CONSOLE);
    }

    @Override
    public boolean isLogToConsole() {
        return this._getLogToConsole();
    }

    protected boolean _getLogToFile() {
        return this.storage.getBoolean("log.chat.to-file", LOG_TO_FILE);
    }

    @Override
    public boolean isLogToFile() {
        return this._getLogToFile();
    }

    protected boolean _getAutoSave() {
        return this.storage.getBoolean("auto-save.enabled", AUTO_SAVE);
    }

    @Override
    public boolean isAutoSave() {
        return this.save;
    }

    protected long _getSaveDelay() {
        return this.storage.getLong("auto-save.delay", AUTO_SAVE_DELAY);
    }

    @Override
    public long getSaveDelay() {
        return this.saveDelay;
    }

    protected boolean _getInform() {
        return this.storage.getBoolean("inform.enabled", INFORM);
    }

    @Override
    public boolean isInform() {
        return this.inform;
    }

    protected long _getInformDelay() {
        return this.storage.getLong("inform.delay", INFORM_DELAY);
    }

    @Override
    public long getInformDelay() {
        return this.informDelay;
    }

    protected int _getCreateLimit() {
        return this.storage.getInt("create-limit", CREATE_LIMIT);
    }

    @Override
    public int getCreateLimit() {
        return this.createLimit;
    }
}
