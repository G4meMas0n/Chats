package de.g4memas0n.chats.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Representation of a Version, that provides a easier updater implementation.
 * Currently unused.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 20th, 2020
 * changed: April 18th, 2020
 */
@SuppressWarnings("unused")
public class Version implements Comparable<Version> {

    private static final Pattern REGEX = Pattern.compile("^(\\d)\\.(\\d{1,2})(\\.(?<patch>\\d+))?(-b(?<build>\\d+))?$");

    private final int major;
    private final int minor;
    private final int patch;
    private final int build;

    public Version(final int major, final int minor) {
        this.major = major;
        this.minor = minor;
        this.patch = 0;
        this.build = 0;
    }

    public Version(final int major, final int minor, final int patch) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.build = 0;
    }

    public Version(final int major, final int minor, final int patch, final int build) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.build = build;
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

    public final int getBuild() {
        return this.build;
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
    public int compareTo(@NotNull final Version version) {
        if (this.major != version.getMajor()) {
            return Integer.compare(this.major, version.getMajor());
        }

        if (this.minor != version.getMinor()) {
            return Integer.compare(this.minor, version.getMinor());
        }

        if (this.patch != version.getPatch()) {
            return Integer.compare(this.patch, version.getPatch());
        }

        return Integer.compare(this.build, version.getBuild());
    }

    @Override
    public boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final Version other = (Version) object;

        return this.major == other.getMajor()
                && this.minor == other.getMinor()
                && this.patch == other.getPatch()
                && this.build == other.getBuild();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;

        result = prime * result + Integer.hashCode(this.major);
        result = prime * result + Integer.hashCode(this.minor);
        result = prime * result + Integer.hashCode(this.patch);
        result = prime * result + Integer.hashCode(this.build);

        return result;
    }

    @Override
    public @NotNull String toString() {
        final StringBuilder version = new StringBuilder();

        version.append(this.major);
        version.append(".");
        version.append(this.minor);

        if (this.patch > 0) {
            version.append(".").append(this.patch);
        }

        if (this.build > 0) {
            version.append("-b").append(this.build);
        }

        return version.toString();
    }

    public static @NotNull Version fromString(@NotNull final String version) throws IllegalArgumentException {
        final Matcher matcher = REGEX.matcher(version);

        if (!matcher.matches()) {
            throw new IllegalArgumentException("version string does not match regex");
        }

        final int major = Integer.parseInt(matcher.group(1));
        final int minor = Integer.parseInt(matcher.group(2));

        if (matcher.groupCount() == 2) {
            return new Version(major, minor);
        }

        if (matcher.groupCount() == 4) {
            final int patch = Integer.parseInt(matcher.group("patch"));
            final int build = Integer.parseInt(matcher.group("build"));

            return new Version(major, minor, patch, build);
        }

        final String patch = matcher.group("patch");

        if (patch != null) {
            return new Version(major, minor, Integer.parseInt(patch));
        }

        return new Version(major, minor, 0, Integer.parseInt(matcher.group("build")));
    }
}
