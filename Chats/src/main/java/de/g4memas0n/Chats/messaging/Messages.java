package de.g4memas0n.Chats.messaging;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Message class, that is used to receive all localized messages of this plugin.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 11th, 2020
 * changed: March 6th, 2020
 */
public final class Messages {

    private static final String BUNDLE_BASE = "resources/messages";

    private static Messages instance;

    private final File directory;

    private final ResourceBundle defaultBundle;
    private ResourceBundle localBundle;
    private ResourceBundle customBundle;

    public Messages(@NotNull final File directory) {
        this.directory = directory;

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

    public @NotNull String translate(@NotNull final String key) {
        try {
            if (this.customBundle != null) {
                try {
                    return this.customBundle.getString(key);
                } catch (MissingResourceException ex) {
                    return this.localBundle.getString(key);
                }
            } else {
                return this.localBundle.getString(key);
            }
        } catch (MissingResourceException ex) {
            return this.defaultBundle.getString(key);
        }
    }

    public @NotNull String translateBoolean(final boolean bool) {
        return bool ? this.translate("true") : this.translate("false");
    }

    public @NotNull String translateError(@NotNull final String key) {
        return this.translate("errorPrefix") + this.translate(key);
    }

    public @NotNull String format(@NotNull final String key, @NotNull final Object... arguments) {
        if (arguments.length == 0) {
            return this.translate(key);
        }

        final String format = this.translate(key);

        try {
            return MessageFormat.format(format, arguments);
        } catch (IllegalArgumentException ex) {
            return MessageFormat.format(format.replaceAll("\\{(\\D*?)}", "\\[$1\\]"), arguments);
        }
    }

    public @NotNull String formatError(@NotNull final String key, @NotNull final Object... arguments) {
        if (arguments.length == 0) {
            return this.translateError(key);
        }

        return this.translate("errorPrefix") + this.format(key, arguments);
    }

    public static @NotNull String tl(@NotNull final String key, @NotNull final Object... arguments) {
        if (instance == null) {
            return "";
        }

        if (arguments.length == 0) {
            return instance.translate(key);
        } else {
            return instance.format(key, arguments);
        }
    }

    public static @NotNull String tlBool(final boolean bool) {
        if (instance == null) {
            return bool ? "true" : "false";
        }

        return instance.translateBoolean(bool);
    }

    public static @NotNull String tlErr(@NotNull final String key, @NotNull final Object... arguments) {
        if (instance == null) {
            return "";
        }

        if (arguments.length == 0) {
            return instance.translateError(key);
        } else {
            return instance.formatError(key, arguments);
        }
    }

    public @NotNull Locale getLocale() {
        return this.localBundle.getLocale();
    }

    public void setLocale(@NotNull final Locale locale) {
        if (locale.equals(this.getLocale())) {
            return;
        }

        try {
            this.localBundle = ResourceBundle.getBundle(BUNDLE_BASE, locale);
        } catch (MissingResourceException ex) {
            this.localBundle = this.defaultBundle;
        }

        try {
            this.customBundle = ResourceBundle.getBundle(BUNDLE_BASE, locale,
                    new CustomFileClassLoader(this.getClass().getClassLoader(), this.directory));
        } catch (MissingResourceException ex) {
            this.customBundle = null;
        }
    }

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
