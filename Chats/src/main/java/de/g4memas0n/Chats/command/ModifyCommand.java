package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.ChatColor;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Modify Command, extends {@link ModifyingCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 18th, 2020
 * changed: June 20th, 2020
 */
public final class ModifyCommand extends ModifyingCommand {

    public ModifyCommand() {
        super("modify", 4, 1);

        this.addCommand(new RemoveCommand(this));
        this.addCommand(new ResetCommand(this));
        this.addCommand(new SetCommand(this));

        this.setDescription("Modifies a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel modify <channel> (remove|reset|set) <type> [<value>]");
    }

    /**
     * The modify remove command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static final class RemoveCommand extends ModifyingSubCommand {

        private static final int TYPE = 0;

        protected RemoveCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "remove", 1, 1);

            this.setDescription("Removes a setting from a channel.");
            this.setUsage("/channel modify <channel> remove <type>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final ModifyType type = ModifyType.getType(arguments[TYPE]);

                if (type == null || !type.isRemovable()) {
                    sender.sendMessage(Messages.tlErr("invalidType"));
                    return true;
                }

                if (sender.canModify(channel, type)) {
                    if (type == ModifyType.ANNOUNCE_FORMAT) {
                        if (channel.setAnnounceFormat(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.BROADCAST_FORMAT) {
                        if (channel.setBroadcastFormat(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.CHAT_FORMAT) {
                        if (channel.setChatFormat(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.DISTANCE) {
                        if (channel.setDistance(-1)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.OWNER) {
                        if (channel.setOwner(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.PASSWORD) {
                        if (channel.setPassword(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.SHORT_NAME) {
                        if (channel.setShortName(null)) {
                            sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    }

                    return true;
                }

                sender.sendMessage(Messages.tl("modifyDeniedType", Messages.tlType(type), channel.getFullName()));
                return true;
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final String[] arguments) {
            if (arguments.length == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isRemovable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), arguments[TYPE])) {
                            completion.add(type.getIdentifier());
                        }
                    }
                }

                return completion;
            }

            return Collections.emptyList();
        }
    }

    /**
     * The modify reset command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static final class ResetCommand extends ModifyingSubCommand {

        private static final int TYPE = 0;

        protected ResetCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "remove", 1, 1);

            this.setDescription("Resets a setting from a channel.");
            this.setUsage("/channel modify <channel> reset <type>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final ModifyType type = ModifyType.getType(arguments[TYPE]);

                if (type == null || !type.isResettable()) {
                    sender.sendMessage(Messages.tlErr("invalidType"));
                    return true;
                }

                if (sender.canModify(channel, type)) {
                    if (type == ModifyType.COLOR) {
                        if (channel.setColor(null)) {
                            sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.CROSS_WORLD) {
                        if (channel.setCrossWorld(true)) {
                            sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    } else if (type == ModifyType.CUSTOM_FORMAT) {
                        if (channel.setCustomFormat(true)) {
                            sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    }

                    return true;
                }

                sender.sendMessage(Messages.tl("modifyDeniedType", Messages.tlType(type), channel.getFullName()));
                return true;
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final String[] arguments) {
            if (arguments.length == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isResettable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), arguments[TYPE])) {
                            completion.add(type.getIdentifier());
                        }
                    }
                }

                return completion;
            }

            return Collections.emptyList();
        }
    }

    /**
     * The modify set command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static final class SetCommand extends ModifyingSubCommand {

        private static final int TYPE = 0;
        private static final int VALUE = 1;

        protected SetCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "set", 2, -1);

            this.setDescription("Sets a setting of a channel.");
            this.setUsage("/channel modify <channel> set <type> <value>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final ModifyType type = ModifyType.getType(arguments[TYPE]);

                if (type == null || !type.isSettable()) {
                    sender.sendMessage(Messages.tlErr("invalidType"));
                    return true;
                }

                if (sender.canModify(channel, type)) {
                    if (type == ModifyType.ANNOUNCE_FORMAT) {
                        final StringBuilder announce = new StringBuilder();

                        for (int i = VALUE; i < arguments.length; i++) {
                            announce.append(arguments[i]).append(" ");
                        }

                        try {
                            if (channel.setAnnounceFormat(announce.toString().trim())) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidFormat", Messages.tl("announce")));
                        }
                    } else if (type == ModifyType.BROADCAST_FORMAT) {
                        final StringBuilder broadcast = new StringBuilder();

                        for (int i = VALUE; i < arguments.length; i++) {
                            broadcast.append(arguments[i]).append(" ");
                        }

                        try {
                            if (channel.setBroadcastFormat(broadcast.toString().trim())) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidFormat", Messages.tl("broadcast")));
                        }
                    } else if (type == ModifyType.CHAT_FORMAT) {
                        final StringBuilder chat = new StringBuilder();

                        for (int i = VALUE; i < arguments.length; i++) {
                            chat.append(arguments[i]).append(" ");
                        }

                        try {
                            if (channel.setChatFormat(chat.toString().trim())) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getChatFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getChatFormat()));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidFormat", Messages.tl("chat")));
                        }
                    }

                    if (arguments.length != this.getMinArgs()) {
                        return false;
                    }

                    if (type == ModifyType.COLOR) {
                        try {
                            if (channel.setColor(ChatColor.valueOf(arguments[VALUE].toUpperCase()))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getColor() + channel.getColor().name().toLowerCase()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getColor() + channel.getColor().name().toLowerCase()));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidColor"));
                        }
                    } else if (type == ModifyType.CROSS_WORLD) {
                        try {
                            if (channel.setCrossWorld((this.parseBoolean(arguments[VALUE])))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCrossWorld())));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCrossWorld())));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidBoolean"));
                        }
                    } else if (type == ModifyType.CUSTOM_FORMAT) {
                        try {
                            if (channel.setCustomFormat((this.parseBoolean(arguments[VALUE])))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCustomFormat())));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCustomFormat())));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidBoolean"));
                        }
                    } else if (type == ModifyType.DISTANCE) {
                        try {
                            if (channel.setDistance(Integer.parseInt(arguments[VALUE]))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getDistance()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getDistance()));
                        } catch (NumberFormatException ex) {
                            sender.sendMessage(Messages.tlErr("invalidNumber"));
                        }
                    } else if (type == ModifyType.OWNER) {
                        if (channel.isPersist()) {
                            sender.sendMessage(Messages.tlErr("modifyOwnerPersist"));
                            return true;
                        }

                        final IChatter target = this.getInstance().getChatterManager().getChatter(arguments[VALUE]);

                        if (target == null || !sender.canSee(target)) {
                            sender.sendMessage(Messages.tlErr("playerNotFound", arguments[VALUE]));
                            return true;
                        }

                        if (channel.setOwner(target.getUniqueId())) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), target.getName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), target.getName()));
                    } else if (type == ModifyType.PASSWORD) {
                        if (channel.isDefault()) {
                            sender.sendMessage(Messages.tlErr("modifyPasswordDefault"));
                            return true;
                        }

                        try {
                            if (channel.setPassword(arguments[VALUE])) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getPassword() != null ? channel.getPassword() : arguments[VALUE]));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getPassword() != null ? channel.getPassword() : arguments[VALUE]));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidPassword"));
                        }
                    } else if (type == ModifyType.SHORT_NAME) {
                        try {
                            if (channel.setShortName(arguments[VALUE])) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getShortName()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getShortName()));
                        } catch (IllegalArgumentException ex) {
                            sender.sendMessage(Messages.tlErr("invalidName"));
                        }
                    }

                    return true;
                }

                sender.sendMessage(Messages.tl("modifyDeniedType", Messages.tlType(type), channel.getFullName()));
                return true;
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final String[] arguments) {
            if (arguments.length == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isSettable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), arguments[TYPE])) {
                            completion.add(type.getIdentifier());
                        }
                    }
                }

                return completion;
            }

            if (arguments.length == VALUE + 1) {
                final ModifyType type = ModifyType.getType(arguments[TYPE]);

                if (type == null || !type.isSettable()) {
                    return Collections.emptyList();
                }

                if (sender.canModify(channel, type)) {
                    final List<String> completion = new ArrayList<>();

                    if (type == ModifyType.COLOR) {
                        for (final ChatColor color : ChatColor.values()) {
                            if (!color.isColor()) {
                                continue;
                            }

                            if (StringUtil.startsWithIgnoreCase(color.name(), arguments[VALUE])) {
                                completion.add(color.name().toLowerCase());
                            }
                        }
                    } else if (type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT) {
                        if (StringUtil.startsWithIgnoreCase(Boolean.FALSE.toString(), arguments[VALUE])) {
                            completion.add(Boolean.FALSE.toString());
                        }

                        if (StringUtil.startsWithIgnoreCase(Boolean.TRUE.toString(), arguments[VALUE])) {
                            completion.add(Boolean.TRUE.toString());
                        }
                    } else if (type == ModifyType.OWNER) {
                        for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                            if (!sender.canSee(target) || channel.isOwner(target.getUniqueId())) {
                                continue;
                            }

                            if (StringUtil.startsWithIgnoreCase(target.getName(), arguments[VALUE])) {
                                completion.add(target.getName());
                            }
                        }

                        Collections.sort(completion);
                    }

                    return completion;
                }
            }

            return Collections.emptyList();
        }

        private boolean parseBoolean(@NotNull final String bool) throws IllegalArgumentException {
            if (bool.equalsIgnoreCase(Boolean.FALSE.toString())) {
                return false;
            } else if (bool.equalsIgnoreCase(Boolean.TRUE.toString())) {
                return true;
            } else {
                throw new IllegalArgumentException(bool + " is not a valid boolean");
            }
        }
    }
}
