package de.g4memas0n.Chats.utils;

import de.g4memas0n.Chats.channels.IChannel;
import org.jetbrains.annotations.NotNull;

@Deprecated
public enum Permission {
    CHATTER_PM("chats.chatter.pm", false),
    CHATTER_IGNORE("chats.chatter.ignore", false),
    CHATTER_UNIGNORED("chats.chatter.unIgnored", false),
    CHATTER_SOCIAL_SPY("chats.chatter.socialSpy", false),

    CHANNEL_JOIN("chats.channel.join", true),
    CHANNEL_LEAVE("chats.channel.leave", true),
    CHANNEL_SPEAK("chats.channel.speak", true),
    CHANNEL_EDIT("chats.channel.edit", true);

    private final String permission;
    private final boolean channelPermission;

    Permission(@NotNull final String permission, final boolean channelPermission) {
        this.permission = permission.toLowerCase();
        this.channelPermission = channelPermission;
    }

    @NotNull
    public String getSubPerm(@NotNull final IChannel channel) throws UnsupportedOperationException {
        if (channelPermission) {
            return this.permission + "." + channel.getFullName().toLowerCase();
        } else {
            throw new UnsupportedOperationException("Unsupported Operation for non channel permissions.");
        }
    }

    @Override
    @NotNull
    public String toString() {
        return this.permission;
    }
}