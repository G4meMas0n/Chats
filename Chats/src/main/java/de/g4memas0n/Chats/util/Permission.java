package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;

/**
 * Permission Enum for all permission notes of this plugin.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 8th, 2020
 * changed: February 8th, 2020
 */
public enum Permission {
    ADMIN_RELOAD("admin.reload", true, false, true),
    CHANNEL_BROADCAST("channel.broadcast", true, true, false),
    CHANNEL_COMMAND("channel", false, false, false),
    CHANNEL_CREATE("channel.create", true, false, true),
    CHANNEL_DELETE("channel.delete", true, true, false),
    CHANNEL_FOCUS("channel.focus", true, true, false),
    CHANNEL_INFO("channel.info", true, true, false),
    CHANNEL_JOIN("channel.join", true, true, false),
    CHANNEL_LEAVE("channel.leave", true, true, false),
    CHANNEL_LIST("channel.list", true, false, true),
    CHANNEL_MODIFY("channel.modify", true, true, false),
    CHANNEL_SPEAK("channel.speak", true, true, false),
    CHATTER_IGNORE("chatter.ignore", false, false, false),
    CHATTER_MSG("chatter.msg", false, false, false),
    EXCEPT_IGNORE("except.ignore", true, false, false),
    EXCEPT_MSG("except.msg", true, false, false),
    FORCE_FOCUS("force.focus", true, false, false),
    FORCE_JOIN("force.join", true, false, false),
    FORCE_LEAVE("force.leave", true, false, false),
    HELP("help", false, false, false),
    USE("use", false, false, false);

    private static final String PREFIX = "chats";

    private final String name;
    private final boolean children;
    private final boolean all;
    private final boolean wildcard;

    Permission(@NotNull final String name, final boolean children, final boolean all, final boolean wildcard) {
        this.name = PREFIX + name;
        this.children = children;
        this.all = all;
        this.wildcard = wildcard;
    }

    public boolean hasChildren() {
        return this.children;
    }

    public boolean hasAll() {
        return this.all;
    }

    public boolean hasWildcard() {
        return this.wildcard;
    }

    public @NotNull String getName() {
        return this.name;
    }

    public @NotNull String formChildren(@NotNull final String children) {
        if (!this.hasChildren()) {
            return this.getName();
        }

        return this.getName() + "." + children;
    }

    public @NotNull String formAll() {
        if (!this.hasAll()) {
            return this.getName();
        }

        return this.formChildren("all");
    }

    public @NotNull String formWildcard() {
        if (!hasWildcard()) {
            return this.getName();
        }

        return this.formChildren("*");
    }
}
