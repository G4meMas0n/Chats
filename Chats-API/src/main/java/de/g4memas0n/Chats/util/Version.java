package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Representation of a Version, that provides a easier updater implementation.
 * Currently unused.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 20th, 2020
 * changed: February 20th, 2020
 */
@SuppressWarnings("unused")
public final class Version implements Comparable<Version> {

    private final int major;
    private final int minor;
    private final int patch;
    private final int build;

    private final BuildType type;

    public Version(final int major, final int minor, final int patch) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.build = 0;
        this.type = BuildType.RELEASE;
    }

    public Version(final int major, final int minor, final int patch, final int build, @NotNull final BuildType type) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.build = build;
        this.type = type;
    }

    public int getMajor() {
        return this.major;
    }

    public int getMinor() {
        return this.minor;
    }

    public int getPatch() {
        return this.patch;
    }

    public int getBuild() {
        return this.build;
    }

    public @NotNull BuildType getBuildType() {
        return this.type;
    }

    public boolean newer(@NotNull final Version version) {
        return this.compareTo(version) > 0;
    }

    public boolean newerOrEqual(@NotNull final Version version) {
        return this.compareTo(version) >= 0;
    }

    public boolean older(@NotNull final Version version) {
        return this.compareTo(version) < 0;
    }

    public boolean olderOrEqual(@NotNull final Version version) {
        return this.compareTo(version) <= 0;
    }

    @Override
    public int compareTo(@NotNull final Version version) {
        if (this.getMajor() != version.getMajor()) {
            return Integer.compare(this.getMajor(), version.getMajor());
        }

        if (this.getMinor() != version.getMinor()) {
            return Integer.compare(this.getMinor(), version.getMinor());
        }

        if (this.getPatch() != version.getPatch()) {
            return Integer.compare(this.getPatch(), version.getPatch());
        }

        if (this.getBuildType() != version.getBuildType()) {
            return Integer.compare(this.getBuildType().ordinal(), version.getBuildType().ordinal());
        } else {
            return Integer.compare(this.getBuild(), version.getBuild());
        }
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

        return this.getMajor() == other.getMajor()
                && this.getMinor() == other.getMinor()
                && this.getPatch() == other.getPatch()
                && this.getBuildType() == other.getBuildType()
                && this.getBuild() == other.getBuild();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;

        result = prime * result + Integer.hashCode(this.getMajor());
        result = prime * result + Integer.hashCode(this.getMinor());
        result = prime * result + Integer.hashCode(this.getPatch());
        result = prime * result + Integer.hashCode(this.getBuild());

        return result;
    }

    @Override
    public @NotNull String toString() {
        final StringBuilder builder = new StringBuilder();

        builder.append(this.getMajor());
        builder.append(".");
        builder.append(this.getMinor());
        builder.append(".");
        builder.append(this.getPatch());

        if (this.getBuildType() != BuildType.RELEASE) {
            builder.append("-");
            builder.append(this.getBuildType().getIdentifier());
            builder.append("-");
            builder.append(this.getBuild());
        }

        return builder.toString();
    }

    public static @NotNull Version fromString(@NotNull final String version) throws IllegalArgumentException {
        final String[] components = version.split("-");

        if (components.length != 1 && components.length != 3) {
            throw new IllegalArgumentException("Invalid version string: " + version);
        }

        final String[] parts = components[0].split(".");

        if (parts.length != 3) {
            throw new IllegalArgumentException("Invalid parts string: " + components[0]);
        }

        try {
            final int major = Integer.parseInt(parts[0]);
            final int minor = Integer.parseInt(parts[1]);
            final int patch = Integer.parseInt(parts[2]);

            if (components.length == 3) {
                final BuildType type = BuildType.getType(components[1]);
                final int build = Integer.parseInt(components[2]);

                if (type == null || build < 0) {
                    throw new IllegalArgumentException("Invalid build string: " + components[1] + "-" + components[2]);
                }

                return new Version(major, minor, patch, build, type);
            }

            return new Version(major, minor, patch);
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(ex);
        }
    }

    public enum BuildType {
        SNAPSHOT("snapshot"),
        RELEASE("release");

        private final String identifier;

        BuildType(@NotNull final String identifier) {
            this.identifier = identifier;
        }

        public @NotNull String getIdentifier() {
            return this.identifier;
        }

        public static @Nullable BuildType getType(@NotNull final String identifier) {
            for (final BuildType current : values()) {
                if (current.identifier.equalsIgnoreCase(identifier)) {
                    return current;
                }
            }

            return null;
        }
    }
}
