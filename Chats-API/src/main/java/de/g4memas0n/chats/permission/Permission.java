package de.g4memas0n.chats.permission;

import org.jetbrains.annotations.NotNull;

/**
 * Permission Enum for all permission notes of this plugin.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public enum Permission {

    /**
     * Permissions for banning chatters from channels.
     *
     * <p>Allows to access the "ban" command.</p>
     *
     * <p>Available ban permissions:<br>
     * - {@code chats.ban} (Allows to access the "ban" command)<br>
     * - {@code chats.ban.bypass} (Allows to bypass channel bans)<br>
     * - {@code chats.ban.exempt} (Allows to being exempt from channel bans)</p>
     *
     * <p><i><b>Note:</b> This permission should be a children of {@link Permission#MODERATE}, but can be removed to
     * regulate moderator permissions.</i></p>
     */
    BAN("ban", true),

    /**
     * Permission for broadcast messages to channels.
     *
     * <p>Allows to access the "broadcast" command.</p>
     *
     * <p><i><b>Note:</b> This permission should be a children of {@link Permission#MODERATE}, but can be removed to
     * regulate moderator permissions.</i></p>
     */
    BROADCAST("broadcast", false),

    /**
     * Permission for the main channel command.
     *
     * <p>Allows to access the "channel" command.</p>
     *
     * <p><i><b>Note:</b> All channel permissions should have this as children permission.</i></p>
     */
    CHANNEL("channel", false),

    /**
     * Permission for cleaning up storage files.
     *
     * <p>Allows to access the "cleanup" command.</p>
     */
    CLEANUP("cleanup", false),

    /**
     * Permission for creating channels.
     *
     * <p>Allows to access the "create" command and to create channels of different types.</p>
     *
     * <p>Available create permissions:<br>
     * - {@code chats.create} (Allows to access the "create" command)<br>
     * - {@code chats.create.unlimited} (Allows to create an unlimited count of new channels)<br>
     * - {@code chats.create.type.persist} (Allows to create persistent channels)<br>
     * - {@code chats.create.type.standard} (Allows to create standard channels)<br>
     * - {@code chats.create.type.*} (Allows to create all channel types)</p>
     */
    CREATE("create", true),

    /**
     * Permission for deleting channels.
     *
     * <p>Allows to access the "delete" command and to delete channels of different types.</p>
     *
     * <p>Available delete permissions:<br>
     * - {@code chats.delete} (Allows to access the "delete" command)<br>
     * - {@code chats.delete.type.persist} (Allows to delete persistent channels)<br>
     * - {@code chats.delete.type.standard} (Allows to delete standard channels)<br>
     * - {@code chats.delete.type.*} (Allows to delete all channels)<br>
     * - {@code chats.delete.own} (Allows to delete owning channels)</p>
     */
    DELETE("delete", true),

    /**
     * Permission for focusing channels.
     *
     * <p>Allows to access the "focus" command and to focus persistent channels.</p>
     *
     * <p>Available focus permissions:<br>
     * - {@code chats.focus} (Allows to access the "focus" command)<br>
     * - {@code chats.focus.persist.<channel>} (Allows to focus the specified persistent channel)<br>
     * - {@code chats.focus.persist.all} (Allows to focus all persistent channels)</p>
     */
    FOCUS("focus", true),

    /**
     * Permissions for forced actions.
     *
     * <p>Available force permissions:<br>
     * - {@code chats.force.exempt} (Allows to being exempt from forcing actions)<br>
     * - {@code chats.force.focus.<channel>} (Forces to focus the specified persistent channel on login)<br>
     * - {@code chats.force.join.<channel>} (Forces to join the specified persistent channel on login)<br>
     * - {@code chats.force.leave.<channel>} (Forces to leave the specified persistent channel on login and logout)</p>
     */
    FORCE("force", true),

    /**
     * Permission for the "help" command.
     *
     * <p>Allows to view the help of this plugin.</p>
     */
    HELP("help", false),

    /**
     * Permission for ignoring chatters.
     *
     * <p>Allows to access the "ignore" and the "unignore" command and to ignore and unignore other chatters.</p>
     *
     * <p>Available ignore permissions:<br>
     * - {@code chats.ignore} (Allows to access the "ignore" and "unignore" command)<br>
     * - {@code chats.ignore.bypass} (Allows to bypass ignores)<br>
     * - {@code chats.ignore.exempt} (Allows to being exempt from ignores)</p>
     */
    IGNORE("ignore", true),

    /**
     * Permission for joining channels.
     *
     * <p>Allows to access the "join" command and to join persistent channels.</p>
     *
     * <p>Available join permissions:<br>
     * - {@code chats.join} (Allows to access the "join" command)<br>
     * - {@code chats.join.persist.<channel>} (Allows to join the specified persistent channel)<br>
     * - {@code chats.join.persist.all} (Allows to join all persistent channels)</p>
     */
    JOIN("join", true),

    /**
     * Permission for kicking chatters from channels.
     *
     * <p>Allows to access the "kick" command.</p>
     *
     * <p>Available kick permissions:<br>
     * - {@code chats.kick} (Allows to access the "kick" command)<br>
     * - {@code chats.kick.exempt} (Allows to being exempt from channel kicks)</p>
     *
     * <p><i><b>Note:</b> This permission should be a children of {@link Permission#MODERATE}, but can be removed to
     * regulate moderator permissions.</i></p>
     */
    KICK("kick", true),

    /**
     * Permission for leaving channels.
     *
     * <p>Allows to access the "leave" command and to leave persistent channels.</p>
     *
     * <p>Available leave permissions:<br>
     * - {@code chats.leave} (Allows to access the "leave" command)<br>
     * - {@code chats.leave.persist.<channel>} (Allows to leave the specified persistent channel)<br>
     * - {@code chats.leave.persist.all} (Allows to leave all persistent channels)</p>
     */
    LEAVE("leave", true),

    /**
     * Permission for listing channels.
     *
     * <p>Allows to access the "list" command and to list channels of different types.</p>
     *
     * <p>Available list permissions:<br>
     * - {@code chats.list} (Allows to access the "list" command)<br>
     * - {@code chats.list.type.conversation} (Allows to list conversation channels)<br>
     * - {@code chats.list.type.persist} (Allows to list persistent channels)<br>
     * - {@code chats.list.type.standard} (Allows to list standard channels)<br>
     * - {@code chats.list.type.*} (Allows to list all channel types)</p>
     */
    LIST("list", true),

    /**
     * Permission for moderating channels.
     *
     * <p>Allows to access moderating commands and to moderate persist or standard channels.</p>
     *
     * <p>Available moderating permissions:<br>
     * - {@code chats.moderate} (Allows to access moderating commands)<br>
     * - {@code chats.moderate.own} (Allows to moderate owning channels)<br>
     * - {@code chats.moderate.persist.<channel>} (Allows to moderate the specified persistent channel)<br>
     * - {@code chats.moderate.persist.all} (Allows to moderate all persistent channels)<br>
     * - {@code chats.moderate.standard.all} (Allows to moderate all standard channels)</p>
     */
    MODERATE("moderate", true),

    /**
     * Permission for modifying channels.
     *
     * <p>Allows to access the "modify" command and to modify channels.</p>
     *
     * <p>Available modify permissions:<br>
     * - {@code chats.modify} (Allows to access the "modify" command)<br>
     * - {@code chats.modify.own} (Allows to modify owning channels)<br>
     * - {@code chats.modify.type.announce-format} (Allows to modify the announce format)<br>
     * - {@code chats.modify.type.broadcast-format} (Allows to modify the broadcast format)<br>
     * - {@code chats.modify.type.chat-format} (Allows to modify the chat format)<br>
     * - {@code chats.modify.type.color} (Allows to modify the color)<br>
     * - {@code chats.modify.type.cross-world} (Allows to modify the cross world option)<br>
     * - {@code chats.modify.type.custom-format} (Allows to modify the custom format option)<br>
     * - {@code chats.modify.type.distance} (Allows to modify the distance)<br>
     * - {@code chats.modify.type.moderator} (Allows to modify the moderators)<br>
     * - {@code chats.modify.type.owner} (Allows to modify the owner)<br>
     * - {@code chats.modify.type.password} (Allows to modify the password)<br>
     * - {@code chats.modify.type.short-name} (Allows to modify the short name)<br>
     * - {@code chats.modify.type.*} (Allows to modify all types)<br>
     * - {@code chats.modify.persist.<channel>} (Allows to modify the specified persistent channel)<br>
     * - {@code chats.modify.persist.all} (Allows to modify all persistent channels)<br>
     * - {@code chats.modify.standard.all} (Allows to modify all standard channels)</p>
     *
     * <p><i><b>Note:</b> For owning channels all types, except the owner, can be modified. So modify type permissions
     * are not required for modify owning channels.</i></p>
     */
    MODIFY("modify", true),

    /**
     * Permission for chatting with chatters.
     *
     * <p>Allows to access the "msg" and "reply" command and to chat with other chatters.</p>
     *
     * <p>Available msg permissions:<br>
     * - {@code chats.msg} (Allows to access the "msg" and "reply" command)<br>
     * - {@code chats.msg.exempt} (Allows to being exempt from private messages)<br>
     * - {@code chats.msg.exempt.bypass} (Allows to bypass the private message exemption)</p>
     */
    MSG("msg", true),

    /**
     * Permission for muting chatters in channels.
     *
     * <p>Allows to access the "mute" command.</p>
     *
     * <p>Available mute permissions:<br>
     * - {@code chats.mute} (Allows to access the "mute" command)<br>
     * - {@code chats.mute.bypass} (Allows to bypass channel mutes)<br>
     * - {@code chats.mute.exempt} (Allows to being exempt from mutes)</p>
     *
     * <p><i><b>Note:</b> This permission should be a children of {@link Permission#MODERATE}, but can be removed to
     * regulate moderator permissions.</i></p>
     */
    MUTE("mute", true),

    /**
     * Permissions for unbanning chatters from channels.
     *
     * <p>Allows to access the "pardon" command.</p>
     *
     * <p><i><b>Note:</b> This permission should be a children of {@link Permission#MODERATE}, but can be removed to
     * regulate moderator permissions.</i></p>
     */
    PARDON("pardon", false),

    /**
     * Permission for reloading the plugin.
     *
     * <p>Allows to access the "reload" command and to reload parts of the plugin.</p>
     *
     * <p>Available reload permissions:<br>
     * - {@code chats.reload} (Allows to access the "reload" command)<br>
     * - {@code chats.reload.type.all} (Allows to reload all sections of the plugin)<br>
     * - {@code chats.reload.type.channel} (Allows to reload channels of the plugin)<br>
     * - {@code chats.reload.type.chatter} (Allows to reload chatters of the plugin)<br>
     * - {@code chats.reload.type.config} (Allows to reload the config of the plugin)<br>
     * - {@code chats.reload.type.*} (Allows to reload all storage types)</p>
     */
    RELOAD("reload", true),

    /**
     * Permission for saving the plugin.
     *
     * <p>Allows to access the "save" command and to save parts of the plugin.</p>
     *
     * <p>Available save permissions:<br>
     * - {@code chats.save} (Allows to access the "save" command)<br>
     * - {@code chats.save.type.all} (Allows to save all sections of the plugin)<br>
     * - {@code chats.save.type.channel} (Allows to save channels of the plugin)<br>
     * - {@code chats.save.type.chatter} (Allows to save chatters of the plugin)<br>
     * - {@code chats.save.type.config} (Allows to save the config of the plugin)<br>
     * - {@code chats.save.type.*} (Allows to save all storage types)</p>
     */
    SAVE("save", true),

    /**
     * Permission for social spying.
     *
     * <p>Allows to access the "social-spy" command and to enabled/disabled social spying.</p>
     *
     * <p>Available social-spy permissions:<br>
     * - {@code chats.social-spy} (Allows to access the "social-spy" command)<br>
     * - {@code chats.social-spy.exempt} (Allows to being exempt from social-spies)</p>
     */
    SOCIAL_SPY("social-spy", true),

    /**
     * Permission for speaking in channels.
     *
     * <p>Allows to access the "chat" command and to speak in persistent channels.</p>
     *
     * <p>Available speak permissions:<br>
     * - {@code chats.speak} (Allows to access the "chat" command)<br>
     * - {@code chats.speak.persist.<channel>} (Allows to speak in the specified persistent channel)<br>
     * - {@code chats.speak.persist.all} (Allows to speak in all persistent channels)</p>
     */
    SPEAK("speak", true),

    /**
     * Permission for the main plugin (chats) command.
     *
     * <p>Allows to access the "chats" command.</p>
     *
     * <p><i><b>Note:</b> All chats permissions should have this as children permission.</i></p>
     */
    USE("use", false),

    /**
     * Permission for the "version" command.
     *
     * <p>Allows to view the version of this plugin.</p>
     */
    VERSION("version", false),

    /**
     * Permission for viewing channel information's.
     *
     * <p>Allows to view information's of channels.</p>
     *
     * <p>Available view permissions:<br>
     * - {@code chats.view.type.color} (Allows to view the color)<br>
     * - {@code chats.view.type.cross-world} (Allows to view the cross-world option)<br>
     * - {@code chats.view.type.distance} (Allows to view the distance)<br>
     * - {@code chats.view.type.formats} (Allows to view the formats)<br>
     * - {@code chats.view.type.moderators} (Allows to view the moderators)<br>
     * - {@code chats.view.type.mutes} (Allows to view the mutes)<br>
     * - {@code chats.view.type.owner} (Allows to view the owner)<br>
     * - {@code chats.view.type.password} (Allows to view the password)<br>
     * - {@code chats.view.type.short-name} (Allows to view the short-name)<br>
     * - {@code chats.view.type.type} (Allows to view the channel type)<br>
     * - {@code chats.view.type.*} (Allows to view all types)<br>
     * - {@link Permission#VIEW_INFO}<br>
     * - {@link Permission#VIEW_WHO}</p>
     */
    VIEW("view", true),

    /**
     * Permission for viewing information's of channels.
     *
     * <p>Allows to access the "info" command.</p>
     *
     * <p>Available info permissions:<br>
     * - {@code chats.view.info} (Allows to access the "info" command)<br>
     * - {@code chats.view.info.own} (Allows to view the info's of owning channels)<br>
     * - {@code chats.view.info.persist.<channel>} (Allows to view the info's of the specified persistent channel)<br>
     * - {@code chats.view.info.persist.all} (Allows to view the info's of all persistent channels)<br>
     * - {@code chats.view.info.standard.all} (Allows to view the info's of all standard channels)</p>
     */
    VIEW_INFO(VIEW, "info", true),

    /**
     * Permission for viewing members of channels.
     *
     * <p>Allows to access the "who" command.</p>
     *
     * <p>Available who permissions:<br>
     * - {@code chats.view.who} (Allows to access the the "who" command)<br>
     * - {@code chats.view.who.own} (Allows to view the members of owning channels)<br>
     * - {@code chats.view.who.persist.<channel>} (Allows to view the members of the specified persistent channel)<br>
     * - {@code chats.view.who.persist.all} (Allows to view the members of all persistent channels)<br>
     * - {@code chats.view.who.standard.all} (Allows to view the members of all standard channels)</p>
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

    /**
     * Returns the complete node of this permission.
     *
     * @return the permission node.
     */
    public @NotNull String getNode() {
        return this.node;
    }

    /**
     * Returns the children permission with the given children node.
     *
     * <p>Joins the node of this permission with the children node together and uses {@code .} as join delimiter.</p>
     *
     * @param children the children's node.
     * @return the children permission node.
     */
    public @NotNull String getChildren(@NotNull final String children) {
        if (!this.children) {
            return this.node;
        }

        return this.node + DELIMITER + children;
    }

    /**
     * Returns the children permission with the given children prefix and children node.
     *
     * <p>Joins the node of this permission with the children's prefix and the children's node together and uses
     * {@code .} as join delimiter.</p>
     *
     * @param prefix the children's prefix.
     * @param children the children's node.
     * @return the children permission node.
     */
    public @NotNull String getChildren(@NotNull final String prefix, @NotNull final String children) {
        if (!this.children) {
            return this.node;
        }

        return this.node + DELIMITER + prefix + DELIMITER + children;
    }
}
