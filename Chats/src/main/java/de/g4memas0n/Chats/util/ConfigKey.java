package de.g4memas0n.Chats.util;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.util.Locale;

/**
 * Config key representation, to represent all keys of the plugins configuration file.
 * @param <T> Object of this configuration key.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 24th, 2020
 * changed: February 5th, 2020
 */
public class ConfigKey<T> {

    public static final ConfigKey<ChatColor> COLOR_CHANNEL = createKey("color.channel", ChatColor.WHITE);
    public static final ConfigKey<ChatColor> COLOR_CONVERSATION = createKey("color.conversation", ChatColor.LIGHT_PURPLE);

    public static final ConfigKey<String> DEF_CHANNEL = createKey("default-channel", "Global");
    public static final ConfigKey<String> FORMAT_ANNOUNCE = createKey("format.announce", "{color}{message}");
    public static final ConfigKey<String> FORMAT_BROADCAST = createKey("format.broadcast", "{color}[{bc-prefix}{color}] {message}");
    public static final ConfigKey<String> FORMAT_CHAT = createKey("format.chat", "{color}[{nick}]{sender}{color}: {message}");
    public static final ConfigKey<String> FORMAT_CONVERSATION = createKey("format.conversation", "{color}[{con-address} {con-partner}{color}] {message}");

    public static final ConfigKey<Locale> LOCALE = createKey("locale", Locale.ENGLISH);

    public static final ConfigKey<Boolean> LOG_CHAT = createKey("log.chat", true);
    public static final ConfigKey<Boolean> LOG_COLORED = createKey("log.colored", true);
    public static final ConfigKey<Boolean> LOG_CONVERSATION = createKey("log.conversation", true);
    public static final ConfigKey<Boolean> LOG_DEBUG = createKey("log.debug", false);
    public static final ConfigKey<Boolean> LOG_TO_FILE = createKey("log.to-file", true);
    public static final ConfigKey<Boolean> LOG_TO_CONSOLE = createKey("log.to-console", true);

    private final String path;
    private final T def;

    protected ConfigKey(@NotNull final String path, @NotNull final T def) {
        this.path = path;
        this.def = def;
    }

    public @NotNull String getPath() {
        return this.path;
    }

    public @NotNull T getDefault() {
        return this.def;
    }

    private static <T> @NotNull ConfigKey<T> createKey(@NotNull final String path, @NotNull final T value) {
        return new ConfigKey<>(path, value);
    }
}
