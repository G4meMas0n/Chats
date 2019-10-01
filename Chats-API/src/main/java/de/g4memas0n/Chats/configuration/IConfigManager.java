package de.g4memas0n.Chats.configuration;

import de.g4memas0n.Chats.channel.IChannel;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.util.Locale;

/**
 * Config Manager Interface that defines a config manager representation.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: September 26th, 2019
 */
public interface IConfigManager {

    /**
     * Returns the config storage representation for this setting manager.
     * @return the config storage of this setting manager.
     */
    @NotNull IConfigStorage getConfigStorage();

    /**
     * Returns the file of this config.
     * @return the config file.
     */
    @NotNull File getConfigFile();

    // Config value methods:
    /**
     * Returns the 'conversion-format.color' option of the config.
     * @return the conversion color.
     */
    @NotNull ChatColor getConversionColor();

    /**
     * Returns the 'conversion-format.normal' option of the config.
     * @return the conversion format.
     */
    @NotNull String getConversionFormat();

    /**
     * Returns the 'conversion-format.twitter-style' option of the config.
     * @return the conversion twitter-style format.
     */
    @NotNull String getTwitterStyleFormat();

    /**
     * Returns the 'conversion-format.use-twitter-style' option of the config.
     * @return the boolean value of this option.
     */
    boolean isUseTwitterStyle();

    /**
     * Returns the 'default-channel' option of the config.
     * @return the full name of the default channel.
     */
    @NotNull String getDefaultChannel();

    /**
     * Sets the 'default-channel' option for the config.
     * @param channel the new default channel.
     * @return true when the option was changed as result of this call.
     * @throws IllegalArgumentException Thrown when the given channel isn't a persist channel.
     */
    boolean setDefaultChannel(@NotNull final IChannel channel) throws IllegalArgumentException;

    /**
     * Returns the 'default-format.announce' option of the config.
     * @return the default announce format.
     */
    @NotNull String getDefaultAnnounceFormat();

    /**
     * Returns the 'default-format.broadcast' option of the config.
     * @return the default broadcast format.
     */
    @NotNull String getDefaultBroadcastFormat();

    /**
     * Returns the 'default-format.channel' option of the config.
     * @return the default channel format.
     */
    @NotNull String getDefaultChannelFormat();

    /**
     * Returns the 'locale' option of the config.
     * @return the current locale.
     */
    @NotNull Locale getLocale();

    /**
     * Returns the 'logs.colored' option of the config.
     * @return the boolean value of this option.
     */
    boolean isLogColored();

    /**
     * Returns the 'logs.to-console' option of the config.
     * @return the boolean value of this option.
     */
    boolean isLogToConsole();

    /**
     * Returns the 'logs.to-file' option of the config.
     * @return the boolean value of this option.
     */
    boolean isLogToFile();
}
