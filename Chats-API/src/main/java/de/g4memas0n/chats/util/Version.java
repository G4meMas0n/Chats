package de.g4memas0n.chats.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Representation of a Version, that provides a easier updater implementation.
 *
 * <p><i><b>Note:</b> Currently unused.</i></p>
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
@SuppressWarnings("unused")
public class Version implements Comparable<Version> {

    private static final Pattern REGEX = Pattern.compile("^(\\d)\\.(\\d{1,2})(\\.(?<patch>\\d+))?(-pre(?<release>\\d+))?$");

    private final int major;
    private final int minor;
    private final int patch;

    private final int release;

    public Version(final int major, final int minor) {
        this.major = major;
        this.minor = minor;
        this.patch = 0;

        this.release = 0;
    }

    public Version(final int major, final int minor, final int patch) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;

        this.release = 0;
    }

    public Version(final int major, final int minor, final int patch, final int release) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;

        this.release = release;
    }

    public final int getMajor() {
        return this.major;
    }

    public final int getMinor() {
        return this.minor;
    }

    public final int getPatch() {
        return this.patch;
    }

    public final int getRelease() {
        return this.release;
    }

    public final boolean newer(@NotNull final Version version) {
        return this.compareTo(version) > 0;
    }

    public final boolean newerOrEqual(@NotNull final Version version) {
        return this.compareTo(version) >= 0;
    }

    public final boolean older(@NotNull final Version version) {
        return this.compareTo(version) < 0;
    }

    public final boolean olderOrEqual(@NotNull final Version version) {
        return this.compareTo(version) <= 0;
    }

    @Override
    public final int compareTo(@NotNull final Version version) {
        if (this.major != version.major) {
            return Integer.compare(this.major, version.major);
        }

        if (this.minor != version.minor) {
            return Integer.compare(this.minor, version.minor);
        }

        if (this.patch != version.patch) {
            return Integer.compare(this.patch, version.patch);
        }

        if (this.release == 0 && version.release > 0) {
            return 1;
        }

        if (this.release > 0 && version.release == 0) {
            return -1;
        }

        return 0;
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (object instanceof Version) {
            final Version other = (Version) object;

            return this.major == other.major
                    && this.minor == other.minor
                    && this.patch == other.patch
                    && this.release == other.release;
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 31;
        int result = 1;

        result = prime * result + Integer.hashCode(this.major);
        result = prime * result + Integer.hashCode(this.minor);
        result = prime * result + Integer.hashCode(this.patch);
        result = prime * result + Integer.hashCode(this.release);

        return result;
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder version = new StringBuilder();

        version.append(this.major);
        version.append(".");
        version.append(this.minor);
        version.append(".");
        version.append(this.patch);

        if (this.release > 0) {
            version.append("-pre");
            version.append(this.release);
        }

        return version.toString();
    }

    public static @NotNull Version fromString(@NotNull final String version) throws IllegalArgumentException {
        final Matcher matcher = REGEX.matcher(version);

        if (!matcher.matches()) {
            throw new IllegalArgumentException("version string does not match regex");
        }

        final int major = Integer.parseUnsignedInt(matcher.group(1));
        final int minor = Integer.parseUnsignedInt(matcher.group(2));

        if (matcher.groupCount() == 2) {
            return new Version(major, minor);
        }

        if (matcher.groupCount() == 4) {
            final int patch = Integer.parseUnsignedInt(matcher.group("patch"));
            final int release = Integer.parseUnsignedInt(matcher.group("release"));

            return new Version(major, minor, patch, release);
        }

        final String patch = matcher.group("patch");

        if (patch != null) {
            return new Version(major, minor, Integer.parseUnsignedInt(patch));
        }

        return new Version(major, minor, 0, Integer.parseUnsignedInt(matcher.group("release")));
    }
}
