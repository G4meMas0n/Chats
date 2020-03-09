package de.g4memas0n.Chats.storage.configuration;

import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.storage.IStorageFile;
import de.g4memas0n.Chats.storage.InvalidStorageFileException;
import de.g4memas0n.Chats.storage.YamlStorageFile;
import de.g4memas0n.Chats.util.logging.Log;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.Locale;

/**
 * Representation of the settings, implements {@link ISettings}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 12th, 2020
 * changed: March 8th, 2020
 */
public final class Settings implements ISettings {

    private static final String FILE_CONFIG = "config.yml";

    private final IStorageFile storage;
    private final IChats instance;

    private ChatColor colorChannel;
    private ChatColor colorConversation;

    private String formatAnnounce;
    private String formatBroadcast;
    private String formatChat;
    private String formatConversation;

    private String defaultChannel;

    private boolean logChat;
    private boolean logColored;
    private boolean logDebug;

    public Settings(@NotNull final IChats instance) {
        this.storage = new YamlStorageFile(new File(instance.getDataFolder(), FILE_CONFIG));
        this.instance = instance;

        this.load();
    }

    @Override
    public @NotNull IStorageFile getStorageFile() {
        return this.storage;
    }

    @Override
    public void delete() {
        try {
            this.storage.delete();
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to delete config file: " + this.storage.getFile().getName());
        }
    }

    @Override
    public void load() {
        if (!this.storage.getFile().exists()) {
            Log.getPluginLogger().severe("Unable to find config file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Saving default configuration...");
            this.instance.saveResource(FILE_CONFIG, true);
        }

        try {
            this.storage.load();
        } catch (IOException | InvalidStorageFileException ex) {
            Log.getPluginLogger().severe("Unable to load config file: " + this.storage.getFile().getName());
            Log.getPluginLogger().info("Using default configuration...");

            this.storage.clear();
        }

        this.formatAnnounce = this._getAnnounceFormat();
        this.formatBroadcast = this._getBroadcastFormat();
        this.formatChat = this._getChatFormat();
        this.formatConversation = this._getConversationFormat();

        this.defaultChannel = this._getDefaultChannel();

        this.colorChannel = this._getChannelColor();
        this.colorConversation = this._getConversationColor();

        this.logChat = this._isLogChat();
        this.logColored = this._isLogColored();
        this.logDebug = this._isLogDebug();
    }

    @Override
    public void save() {
        this._setAnnounceFormat(this.getAnnounceFormat());
        this._setBroadcastFormat(this.getBroadcastFormat());
        this._setChatFormat(this.getChatFormat());
        this._setConversationFormat(this.getConversationFormat());

        this._setDefaultChannel(this.getDefaultChannel());

        this._setChannelColor(this.getChannelColor());
        this._setConversationColor(this.getConversationColor());

        this._setLocale(this.getLocale());

        this._setLogChat(this.isLogChat());
        this._setLogColored(this.isLogColored());
        this._setLogDebug(this.isLogDebug());
        this._setLogToConsole(this.isLogToConsole());
        this._setLogToFile(this.isLogToFile());

        try {
            this.storage.save();
        } catch (IOException ex) {
            Log.getPluginLogger().warning("Unable to save config file: " + this.storage.getFile().getName());
        }
    }

    // Format Settings:
    @Override
    public @NotNull String getAnnounceFormat() {
        if (this.formatAnnounce == null) {
            this.formatAnnounce = this._getAnnounceFormat();
        }

        return this.formatAnnounce;
    }

    private @NotNull String _getAnnounceFormat() {
        final String format = this.storage.getString("format.announce");

        if (format == null || format.isEmpty()) {
            return "{color}{message}";
        }

        return format;
    }

    private void _setAnnounceFormat(@NotNull final String format) {
        this.storage.set("format.announce", format);
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        if (this.formatBroadcast == null) {
            this.formatBroadcast = this._getBroadcastFormat();
        }

        return this.formatBroadcast;
    }

    private @NotNull String _getBroadcastFormat() {
        final String format = this.storage.getString("format.broadcast");

        if (format == null || format.isEmpty()) {
            return "{color}[{bc-prefix}{color}] {message}";
        }

        return format;
    }

    private void _setBroadcastFormat(@NotNull final String format) {
        this.storage.set("format.broadcast", format);
    }

    @Override
    public @NotNull String getChatFormat() {
        if (this.formatChat == null) {
            this.formatChat = this._getChatFormat();
        }

        return this.formatChat;
    }

    private @NotNull String _getChatFormat() {
        final String format = this.storage.getString("format.chat");

        if (format == null || format.isEmpty()) {
            return "{color}[{nick}]{sender}{color}: {message}";
        }

        return format;
    }

    private void _setChatFormat(@NotNull final String format) {
        this.storage.set("format.chat", format);
    }

    @Override
    public @NotNull String getConversationFormat() {
        if (this.formatConversation == null) {
            this.formatConversation = this._getConversationFormat();
        }

        return this.formatConversation;
    }

    private @NotNull String _getConversationFormat() {
        final String format = this.storage.getString("format.conversation");

        if (format == null || format.isEmpty()) {
            return "{color}[{con-address} {con-partner}{color}] {message}";
        }

        return format;
    }

    private void _setConversationFormat(@NotNull final String format) {
        this.storage.set("format.conversation", format);
    }

    // Default Channel Setting:
    @Override
    public @NotNull String getDefaultChannel() {
        if (this.defaultChannel == null) {
            this.defaultChannel = this._getDefaultChannel();
        }

        return this.defaultChannel;
    }

    private @NotNull String _getDefaultChannel() {
        final String fullName = this.storage.getString("default-channel");

        if (fullName == null || fullName.isEmpty()) {
            return "Global";
        }

        return fullName;
    }

    @Override
    public void setDefaultChannel(@NotNull final String fullName) {
        if (fullName.equals(this.defaultChannel)) {
            return;
        }

        this.defaultChannel = fullName;
        this._setDefaultChannel(fullName);
    }

    private void _setDefaultChannel(@NotNull final String fullName) {
        this.storage.set("default-channel", fullName);
    }

    // Color Settings:
    @Override
    public @NotNull ChatColor getChannelColor() {
        if (this.colorChannel == null) {
            this.colorChannel = this._getChannelColor();
        }

        return this.colorChannel;
    }

    private @NotNull ChatColor _getChannelColor() {
        final ChatColor color = this.storage.getChatColor("color.channel");

        if (color == null || !color.isColor()) {
            return ChatColor.WHITE;
        }

        return color;
    }

    private void _setChannelColor(@NotNull final ChatColor color) {
        this.storage.set("color.channel", color);
    }

    @Override
    public @NotNull ChatColor getConversationColor() {
        if (this.colorConversation == null) {
            this.colorConversation = _getConversationColor();
        }

        return this.colorConversation;
    }

    private @NotNull ChatColor _getConversationColor() {
        final ChatColor color = this.storage.getChatColor("color.conversation");

        if (color == null || !color.isColor()) {
            return ChatColor.LIGHT_PURPLE;
        }

        return color;
    }

    private void _setConversationColor(@NotNull final ChatColor color) {
        this.storage.set("color.conversation", color);
    }

    // Locale Setting:
    @Override
    public @NotNull Locale getLocale() {
        return this._getLocale();
    }

    private @NotNull Locale _getLocale() {
        final Locale locale = this.storage.getLocale("locale");

        if (locale == null) {
            return Locale.ENGLISH;
        }

        return locale;
    }

    private void _setLocale(@NotNull final Locale locale) {
        this.storage.set("locale", locale);
    }

    // Log Settings:
    @Override
    public boolean isLogChat() {
        return this.logChat || this._isLogChat();
    }

    private boolean _isLogChat() {
        return this.storage.getBoolean("log.chat", true);
    }

    private void _setLogChat(final boolean logChat) {
        this.storage.set("log.chat", logChat);
    }

    @Override
    public boolean isLogColored() {
        return this.logColored || this._isLogColored();
    }

    private boolean _isLogColored() {
        return this.storage.getBoolean("log.colored", true);
    }

    private void _setLogColored(final boolean logColored) {
        this.storage.set("log.colored", logColored);
    }

    @Override
    public boolean isLogDebug() {
        return this.logDebug || this._isLogDebug();
    }

    private boolean _isLogDebug() {
        return this.storage.getBoolean("log.debug", false);
    }

    private void _setLogDebug(final boolean logDebug) {
        this.storage.set("log.debug", logDebug);
    }

    @Override
    public boolean isLogToConsole() {
        return this._isLogToConsole();
    }

    private boolean _isLogToConsole() {
        return this.storage.getBoolean("log.to-console", true);
    }

    private void _setLogToConsole(final boolean logToConsole) {
        this.storage.set("log.to-console", logToConsole);
    }

    @Override
    public boolean isLogToFile() {
        return this._isLogToFile();
    }

    private boolean _isLogToFile() {
        return this.storage.getBoolean("log.to-file", true);
    }

    private void _setLogToFile(final boolean logToFile) {
        this.storage.set("log.to-file", logToFile);
    }
}
