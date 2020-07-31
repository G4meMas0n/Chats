package de.g4memas0n.chats.messaging;

import de.g4memas0n.chats.util.logging.BasicLogger;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Message class, that is used to receive all localized messages of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class Messages {

    private static final String BUNDLE_BASE = "resources/messages";

    private static Messages instance;

    private final BasicLogger logger;
    private final File directory;

    private final ResourceBundle defaultBundle;
    private ResourceBundle localBundle;
    private ResourceBundle customBundle;

    public Messages(@NotNull final File directory, @NotNull final BasicLogger logger) {
        this.directory = directory;
        this.logger = logger;

        this.defaultBundle = ResourceBundle.getBundle(BUNDLE_BASE);
        this.localBundle = this.defaultBundle;
        this.customBundle = null;
    }

    public void enable() {
        instance = this;
    }

    public void disable() {
        instance = null;
    }

    public synchronized @NotNull Locale getLocale() {
        return this.customBundle != null ? this.customBundle.getLocale() : this.localBundle.getLocale();
    }

    public synchronized void setLocale(@NotNull final Locale locale) {
        try {
            this.localBundle = ResourceBundle.getBundle(BUNDLE_BASE, locale);

            if (this.localBundle.getLocale().equals(locale)) {
                this.logger.debug("Loaded resource bundle for language: " + locale);
            } else {
                this.logger.warning("Unable to find resource bundle for language: " + locale);
                this.logger.debug("Loaded fallback resource bundle for language: " + this.localBundle.getLocale());
            }
        } catch (MissingResourceException ex) {
            this.localBundle = this.defaultBundle;
            this.logger.warning("Unable to find resource bundle. Using default bundle.");
        }

        try {
            this.customBundle = ResourceBundle.getBundle(BUNDLE_BASE, locale,
                    new CustomFileClassLoader(this.getClass().getClassLoader(), this.directory),
                    ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES));

            if (this.customBundle.getLocale().equals(locale)) {
                this.logger.debug("Detected and loaded custom resource bundle for language: " + locale);
            } else {
                this.customBundle = null;
            }
        } catch (MissingResourceException ex) {
            this.customBundle = null;
        }

        this.logger.info(String.format("Locale has been changed. Using locale %s", this.getLocale()));
    }

    public synchronized @NotNull String translate(@NotNull final String key) {
        try {
            if (this.customBundle != null) {
                try {
                    return this.customBundle.getString(key);
                } catch (MissingResourceException ex) {
                    this.logger.debug(String.format("Missing translation key '%s' in custom translation file for language: %s",
                            ex.getKey(), this.customBundle.getBaseBundleName()));
                }
            }

            return this.localBundle.getString(key);
        } catch (MissingResourceException ex) {
            this.logger.debug(String.format("Missing translation key '%s' in translation file for language: %s",
                    ex.getKey(), this.getLocale()));

            return this.defaultBundle.getString(key);
        }
    }

    public synchronized @NotNull String format(@NotNull final String key,
                                               @NotNull final Object... arguments) {
        if (arguments.length == 0) {
            return this.translate(key);
        }

        final String format = this.translate(key);

        try {
            return MessageFormat.format(format, arguments);
        } catch (IllegalArgumentException ex) {
            this.logger.warning("Invalid translation key '%s': " + ex.getMessage());

            return MessageFormat.format(format.replaceAll("\\{(\\D*?)}", "\\[$1\\]"), arguments);
        }
    }

    public synchronized @NotNull String formatJoining(@NotNull final String key,
                                                      @NotNull final Collection<String> collection) {
        if (collection.isEmpty()) {
            return this.translate(key);
        }

        final String format = this.translate(key);
        final String delimiter = this.translate("delimiter") + ChatColor.getLastColors(format);
        final String joined = String.join(delimiter, collection);

        try {
            return MessageFormat.format(format, joined);
        } catch (IllegalArgumentException ex) {
            this.logger.warning("Invalid translation key '%s': " + ex.getMessage());

            return MessageFormat.format(format.replaceAll("\\{(\\D*?)}", "\\[$1\\]"), joined);
        }
    }

    public static @NotNull String tl(@NotNull final String key,
                                     @NotNull final Object... arguments) {
        if (instance == null) {
            return "\u00a74Error: \u00a7cMessages not loaded.";
        }

        return instance.format(key, arguments);
    }

    public static @NotNull String tlBool(final boolean bool) {
        if (instance == null) {
            return Boolean.toString(bool);
        }

        return instance.translate(Boolean.valueOf(bool).toString());
    }

    public static @NotNull String tlJoin(@NotNull final String key,
                                         @NotNull final Collection<String> collection) {
        if (instance == null) {
            return "\u00a74Error: \u00a7cMessages not loaded.";
        }

        return instance.formatJoining(key, collection);
    }

    public static @NotNull String tlState(final boolean state) {
        if (instance == null) {
            return state ? "enabled" : "disabled";
        }

        return instance.translate(state ? "enabled" : "disabled");
    }

    /**
     * Custom ClassLoader for getting resource bundles located in the plugins data folder.
     */
    private static class CustomFileClassLoader extends ClassLoader {

        private final File directory;

        private CustomFileClassLoader(@NotNull final ClassLoader loader, @NotNull final File directory) {
            super(loader);

            this.directory = directory;
        }

        @Override
        public @Nullable URL getResource(@NotNull final String name) {
            final File file = new File(this.directory, name);

            if (file.exists()) {
                try {
                    return file.toURI().toURL();
                } catch (MalformedURLException ignored) {

                }
            }

            return null;
        }

        @Override
        public @Nullable InputStream getResourceAsStream(@NotNull final String name) {
            final File file = new File(this.directory, name);

            if (file.exists()) {
                try {
                    return new FileInputStream(file);
                } catch (FileNotFoundException ignored) {

                }
            }

            return null;
        }
    }
}
