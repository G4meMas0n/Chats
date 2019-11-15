package de.g4memas0n.Chats.configuration;

import de.g4memas0n.Chats.channel.IChannel;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.util.Locale;

/**
 * Representation of the config manager, implements the {@link IConfigManager} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: November 13th, 2019
 */
public final class ConfigManager implements IConfigManager {

    /**
     * count variable, that makes the config manager differentiable.
     */
    private static int count = 0;

    private final IConfigStorage configStorage;
    private final File configFile;
    private final int id;

    private ChatColor conColor = ChatColor.LIGHT_PURPLE;

    private String conFormat = "{color}[{sender}{color} -> {con-partner}{color}] {message}";
    private String conFormatTwitterStyle = "{color}[{con-address} {con-partner}{color}] {message}";
    private String defaultChannel;
    private String defFormatAnnounce = "{color}{message}";
    private String defFormatBroadcast = "{color}[{bc-prefix}{color}] {message}";
    private String defFormatChannel = "{color}[{nick}]{sender}{color}: {message}";

    private Locale locale = Locale.ENGLISH;

    private boolean conFormatUseTwitterStyle = true;
    private boolean logsColored = false;
    private boolean logsDebug = false;
    private boolean logsToConsole = true;
    private boolean logsToFile = true;

    protected ConfigManager(@NotNull final IConfigStorage configStorage,
                            @NotNull final File configFile,
                            @NotNull final String defaultChannelName) {
        this.configStorage = configStorage;
        this.configFile = configFile;
        this.id = count++;
        this.defaultChannel = defaultChannelName;
    }

    @Override
    public @NotNull IConfigStorage getConfigStorage() {
        return this.configStorage;
    }

    @Override
    public @NotNull File getConfigFile() {
        return this.configFile;
    }

    // Config value methods:
    @Override
    public @NotNull ChatColor getConversionColor() {
        return this.conColor;
    }

    protected void _setConversionColor(@NotNull final ChatColor color) {
        if (this.conColor == color) {
            return;
        }

        this.conColor = color;
    }

    @Override
    public @NotNull String getConversionFormat() {
        return this.conFormat;
    }

    protected void _setConversionFormat(@NotNull final String format) {
        if (this.conFormat.equals(format)) {
            return;
        }

        this.conFormat = format;
    }

    @Override
    public @NotNull String getTwitterStyleFormat() {
        return this.conFormatTwitterStyle;
    }

    protected void _setTwitterStyleFormat(@NotNull final String format) {
        if (this.conFormatTwitterStyle.equals(format)) {
            return;
        }

        this.conFormatTwitterStyle = format;
    }

    @Override
    public boolean isUseTwitterStyle() {
        return this.conFormatUseTwitterStyle;
    }

    protected void _setUseTwitterStyle(final boolean enabled) {
        if (this.conFormatUseTwitterStyle == enabled) {
            return;
        }

        this.conFormatUseTwitterStyle = enabled;
    }

    @Override
    public @NotNull String getDefaultChannel() {
        return this.defaultChannel;
    }

    @Override
    public boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException {
        if (!channel.isPersistChannel()) {
            throw new IllegalArgumentException("Invalid Channel. Channel must be a persist channel!");
        }

        if (this.defaultChannel.equals(channel.getFullName())) {
            return false;
        }

        this.defaultChannel = channel.getFullName();
        this.configStorage.update(this);
        return true;
    }

    protected void _setDefaultChannel(@NotNull final String channelName) {
        if (this.defaultChannel.equals(channelName)) {
            return;
        }

        this.defaultChannel = channelName;
    }

    @Override
    public @NotNull String getDefaultAnnounceFormat() {
        return this.defFormatAnnounce;
    }

    protected void _setDefaultAnnounceFormat(@NotNull final String format) {
        if (this.defFormatAnnounce.equals(format)) {
            return;
        }

        this.defFormatAnnounce = format;
    }

    @Override
    public @NotNull String getDefaultBroadcastFormat() {
        return this.defFormatBroadcast;
    }

    protected void _setDefaultBroadcastFormat(@NotNull final String format) {
        if (this.defFormatBroadcast.equals(format)) {
            return;
        }

        this.defFormatBroadcast = format;
    }

    @Override
    public @NotNull String getDefaultChannelFormat() {
        return this.defFormatChannel;
    }

    protected void _setDefaultChannelFormat(@NotNull final String format) {
        if (this.defFormatChannel.equals(format)) {
            return;
        }

        this.defFormatChannel = format;
    }

    @Override
    public @NotNull Locale getLocale() {
        return this.locale;
    }

    protected void _setLocale(@NotNull final Locale locale) {
        if (this.locale.equals(locale)) {
            return;
        }

        this.locale = locale;
    }

    @Override
    public boolean isLogColored() {
        return this.logsColored;
    }

    protected void _setLogColored(final boolean enabled) {
        if (this.logsColored == enabled) {
            return;
        }

        this.logsColored = enabled;
    }

    @Override
    public boolean isLogDebug() {
        return this.logsDebug;
    }

    protected void _setLogDebug(final boolean enabled) {
        if (this.logsDebug == enabled) {
            return;
        }

        this.logsDebug = enabled;
    }

    @Override
    public boolean isLogToConsole() {
        return this.logsToConsole;
    }

    protected void _setLogToConsole(final boolean enabled) {
        if (this.logsToConsole == enabled) {
            return;
        }

        this.logsToConsole = enabled;
    }

    @Override
    public boolean isLogToFile() {
        return this.logsToFile;
    }

    protected void _setLogToFile(final boolean enabled) {
        if (this.logsToFile == enabled) {
            return;
        }

        this.logsToFile = enabled;
    }

    @Override
    public boolean equals(@NotNull final Object object) {
        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final ConfigManager manager = (ConfigManager) object;
        return this.id == manager.id;
    }

    @Override
    public int hashCode() {
        final int prime = 41;
        int result = 3;

        result = prime * result + Integer.hashCode(this.id);

        return result;
    }
}
