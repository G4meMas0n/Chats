package de.g4memas0n.chats.util;

import org.jetbrains.annotations.NotNull;

/**
 * Permission Enum for all permission notes of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 8th, 2020
 * changed: June 22th, 2020
 */
public enum Permission {

    /**
     * Permissions for banning chatters from channels. Allows to access the "ban" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be removed
     * to regulate moderator permissions.
     * Available ban permissions:
     * - chats.ban (Allows to access the "ban" command)
     * - chats.ban.exempt (Allows to being exempt from channel bans)
     */
    BAN("ban", true),

    /**
     * Permission for broadcast messages to channels. Allows to access the "broadcast" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be remove
     * to regulate moderator permissions.
     */
    BROADCAST("broadcast", false),

    /**
     * Permission for the main channel command. Allows to use the "channel" command.
     * All channel permissions should have this as children permission.
     */
    CHANNEL("channel", false),

    /**
     * Permission for cleaning up storage files. Allows to use the "cleanup" command.
     */
    CLEANUP("cleanup", false),

    /**
     * Permission  for creating channels. Allows to access the "create" command and to create channels of different
     * types.
     * Available create permissions:
     * - chats.create (Allows to access the "create" command)
     * - chats.create.limit (Limits the channel creation, that max one channel can be created)
     * - chats.create.type.persist (Allows to create persistent channels)
     * - chats.create.type.standard (Allows to create standard channels)
     * - chats.create.type.* (Allows to create all channel types)
     */
    CREATE("create", true),

    /**
     * Permission for deleting channels. Allows to access the "delete" command and to delete channels of different
     * types.
     * Available delete permissions:
     * - chats.delete
     * - chats.delete.type.persist
     * - chats.delete.type.standard
     * - chats.delete.type.*
     * - chats.delete.own
     */
    DELETE("delete", true),

    /**
     * Permission for focusing channels. Allows to access the "focus" command and to focus persistent channels.
     * Available focus permissions:
     * - chats.focus (Allows to access the "focus" command)
     * - chats.focus.persist.<FullName> (Allows to focus a persist channel with name <FullName>)
     * - chats.focus.persist.all (Allows to focus all persistent channels)
     */
    FOCUS("focus", true),

    /**
     * Permissions for forced actions.
     * Available force permissions:
     * - chats.force.focus.<FullName> (Forces to focus a persist channel with name <FullName> on login)
     * - chats.force.join.<FullName> (Forces to join a persist channel with name <FullName> on login)
     * - chats.force.leave.<FullName> (Forces to leave a persist channel with name <FullName> on logout)
     */
    FORCE("force", true),

    /**
     * Permission for the "help" command. Allows to view the help of this plugin.
     */
    HELP("help", false),

    /**
     * Permission for ignoring chatters. Allows to access the "ignore" and the "unignore" command and to ignore and
     * unignore other chatters.
     * Available ignore permissions:
     * - chats.ignore (Allows to access the "ignore" and "unignore" command)
     * - chats.ignore.exempt (Allows to being exempt from ignores)
     */
    IGNORE("ignore", true),

    /**
     * Permission for joining channels. Allows to access the "join" command and to join persistent channels.
     * Available join permissions:
     * - chats.join (Allows to access the "join" command)
     * - chats.join.persist.<FullName> (Allows to join a persist channel with name <FullName>)
     * - chats.join.persist.all (Allows to join all persistent channels)
     */
    JOIN("join", true),

    /**
     * Permission for kicking chatters from channels. Allows to access the "kick" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be removed
     * to regulate moderator permissions.
     * Available kick permissions:
     * - chats.kick (Allows to access the "kick" command)
     * - chats.kick.exempt (Allows to being exempt from channel kicks)
     */
    KICK("kick", true),

    /**
     * Permission for leaving channels. Allows to access the "leave" command and to leave persistent channels.
     * Available leave permissions:
     * - chats.leave (Allows to access the "leave" command)
     * - chats.leave.persist.<FullName> (Allows to leave a persist channel with name <FullName>)
     * - chats.leave.persist.all (Allows to leave all persistent channels)
     */
    LEAVE("leave", true),

    /**
     * Permission for listing channels. Allows to access the "list" command and to list channels of different types.
     * Available list permissions:
     * - chats.list (Allows to access the "list" command)
     * - chats.list.type.conversation (Allows to list conversation channels)
     * - chats.list.type.persist (Allows to list persistent channels)
     * - chats.list.type.standard (Allows to list standard channels)
     * - chats.list.type.* (Allows to list all channel types)
     */
    LIST("list", true),

    /**
     * Permission for moderating channels. Allows to access moderating commands and to moderate persist or standard
     * channels.
     * Available moderating permissions:
     * - chats.moderate (Allows to access moderating commands)
     * - chats.moderate.own (Allows to moderate owning channels)
     * - chats.moderate.persist.<FullName> (Allows to moderate a persist channel with name <FullName>)
     * - chats.moderate.persist.all (Allows to moderate all persistent channels)
     * - chats.moderate.standard.all (Allows to moderate all standard channels)
     */
    MODERATE("moderate", true),

    /**
     * Permission for modifying channels. Allows to access the "modify" command and to modify channels.
     * Note: For owning channels all types, except the owner, can be modified. So modify type permissions are not
     * required for modify owning channels.
     * Available modify permissions:
     * - chats.modify
     * - chats.modify.own (Allows to modify owning channels)
     * - chats.modify.type.announce-format (Allows to modify the announce format)
     * - chats.modify.type.broadcast-format (Allows to modify the broadcast format)
     * - chats.modify.type.chat-format (Allows to modify the chat format)
     * - chats.modify.type.color (Allows to modify the color)
     * - chats.modify.type.cross-world (Allows to modify the cross world option)
     * - chats.modify.type.custom-format (Allows to modify the custom format option)
     * - chats.modify.type.distance (Allows to modify the distance)
     * - chats.modify.type.owner (Allows to modify the owner)
     * - chats.modify.type.password (Allows to modify the password)
     * - chats.modify.type.short-name (Allows to modify the short name)
     * - chats.modify.type.* (Allows to modify all types)
     * - chats.modify.persist.<FullName> (Allows to modify a persistent channel with name <FullName>)
     * - chats.modify.persist.all (Allows to modify all persistent channels)
     * - chats.modify.standard.all (Allows to modify all standard channels)
     */
    MODIFY("modify", true),

    /**
     * Permission for chatting with chatters. Allows to access the "msg" and "reply" command and to chat with other
     * chatters.
     * Available msg permissions:
     * - chats.msg (Allows to access the "msg" and "reply" command)
     * - chats.msg.exempt (Allows to being exempt from private messages)
     * - chats.msg.exempt.bypass (Allows to bypass the private message exemption)
     */
    MSG("msg", true),

    /**
     * Permission for muting chatters in channels. Allows to access the "mute" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be removed
     * to regulate moderator permissions.
     * Available mute permissions:
     * - chats.mute (Allows to access the "mute" command)
     * - chats.mute.exempt (Allows to being exempt from mutes)
     */
    MUTE("mute", true),

    /**
     * Permissions for unbanning chatters from channels. Allows to access the "pardon" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be removed
     * to regulate moderator permissions.
     */
    PARDON("pardon", false),

    /**
     * Permission for reloading the plugin. Allows to access the "reload" command and to reload parts of the plugin.
     * Available reload permissions:
     * - chats.reload (Allows to access the "reload" command)
     * - chats.reload.type.all (Allows to reload all sections of the plugin)
     * - chats.reload.type.channel (Allows to reload channels of the plugin)
     * - chats.reload.type.chatter (Allows to reload chatters of the plugin)
     * - chats.reload.type.config (Allows to reload the config of the plugin)
     * - chats.reload.type.* (Allows to reload all storage types)
     */
    RELOAD("reload", true),

    /**
     * Permission for saving the plugin. Allows to access the "save" command and to save parts of the plugin.
     * Available save permissions:
     * - chats.save (Allows to access the "save" command)
     * - chats.save.type.all (Allows to save all sections of the plugin)
     * - chats.save.type.channel (Allows to save channels of the plugin)
     * - chats.save.type.chatter (Allows to save chatters of the plugin)
     * - chats.save.type.config (Allows to save the config of the plugin)
     * - chats.save.type.* (Allows to save all storage types)
     */
    SAVE("save", true),

    /**
     * Permission for social spying. Allows to access the "social-spy" command and to enabled/disabled social spying.
     * Available social-spy permissions:
     * - chats.social-spy (Allows to access the "social-spy" command)
     * - chats.social-spy.exempt (Allows to being exempt from social-spies)
     */
    SOCIAL_SPY("social-spy", true),

    /**
     * Permission for speaking in channels. Allows to access the "chat" command and to speak in persistent channels.
     * Available speak permissions:
     * - chats.speak (Allows to access the "chat" command)
     * - chats.speak.persist.<FullName> (Allows to speak in a persist channel with name <FullName>)
     * - chats.speak.persist.all (Allows to speak in all persistent channels)
     */
    SPEAK("speak", true),

    /**
     * Permissions for unmute chatters from channels. Allows to access the "unmute" command.
     * This permission should be a children of {@link Permission#MODERATE}, but can be removed
     * to regulate moderator permissions.
     */
    UNMUTE("unmute", false),

    /**
     * Permission for the main plugin (chats) command. Allows to access the "chats" command.
     * All chats permissions should have this as children permission.
     */
    USE("use", false),

    /**
     * Permission for the "version" command. Allows to view the version of this plugin.
     */
    VERSION("version", false),

    /**
     * Permission for viewing channel information's.
     * Available view permissions:
     * - chats.view.type.color
     * - chats.view.type.cross-world
     * - chats.view.type.distance
     * - chats.view.type.format
     * - chats.view.type.moderators
     * - chats.view.type.mutes
     * - chats.view.type.owner
     * - chats.view.type.password
     * - chats.view.type.short-name
     * - chats.view.type.type
     * - chats.view.type.*
     * - {@link Permission#VIEW_INFO}
     * - {@link Permission#VIEW_WHO}
     */
    VIEW("view", true),

    /**
     * Permission for viewing information's of channels. Allows to access the "info" command.
     * Available info permissions:
     * - chats.view.info (Allows to access the "info" command)
     * - chats.view.info.own (Allows to view the info's of owning channels)
     * - chats.view.info.persist.<FullName> (Allows to view the info's of a persist channel with name <FullName>)
     * - chats.view.info.persist.all (Allows to view the info's of all persistent channels)
     * - chats.view.info.standard.all (Allows to view the info's of all standard channels)
     */
    VIEW_INFO(VIEW, "info", true),

    /**
     * Permission for viewing members of channels. Allows to access the "who" command.
     * - chats.view.who (Allows to access the the "who" command)
     * - chats.view.who.own (Allows to view the members of owning channels)
     * - chats.view.who.persist.<FullName> (Allows to view the members of a persist channel with name <FullName>)
     * - chats.view.who.persist.all (Allows to view the members of all persistent channels)
     * - chats.view.who.standard.all (Allows to view the members of all standard channels)
     */
    VIEW_WHO(VIEW, "who", true);

    private static final String DELIMITER = ".";
    private static final String PREFIX = "chats";

    private final String node;
    private final boolean children;

    Permission(@NotNull final String node, final boolean children) {
        this.node = PREFIX + DELIMITER + node;
        this.children = children;
    }

    Permission(@NotNull final Permission parent, @NotNull final String node, final boolean children) {
        this.node = parent.getNode() + DELIMITER + node;
        this.children = children;
    }

    public @NotNull String getNode() {
        return this.node;
    }

    public @NotNull String formChildren(@NotNull final String children) {
        if (!this.children) {
            return this.node;
        }

        return this.node + DELIMITER + children;
    }

    public @NotNull String formChildren(@NotNull final String prefix, @NotNull final String children) {
        if (!this.children) {
            return this.node;
        }

        return this.node + DELIMITER + prefix + DELIMITER + children;
    }
}
