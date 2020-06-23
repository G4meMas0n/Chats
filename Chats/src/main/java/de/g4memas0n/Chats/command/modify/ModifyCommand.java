package de.g4memas0n.chats.command.modify;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidChannelException;
import de.g4memas0n.chats.util.input.InvalidColorException;
import de.g4memas0n.chats.util.input.InvalidFormatException;
import de.g4memas0n.chats.util.input.InvalidNameException;
import de.g4memas0n.chats.util.input.InvalidPasswordException;
import de.g4memas0n.chats.util.input.InvalidPlayerException;
import de.g4memas0n.chats.util.input.InvalidTypeException;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.ChatColor;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The modify command that allows to modify a channel.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 18th, 2020
 * changed: June 22th, 2020
 */
public final class ModifyCommand extends ModifyingCommand {

    public ModifyCommand() {
        super("modify", 3);

        this.addCommand(new RemoveCommand(this));
        this.addCommand(new ResetCommand(this));
        this.addCommand(new SetCommand(this));

        this.setDescription("Modifies a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel modify <channel> (remove|reset|set) <type> [<value>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new InvalidChannelException(input.get(CHANNEL));
            }

            if (sender.canModify(channel)) {
                final SubCommand command = this.getCommand(input.get(COMMAND));

                if (command == null) {
                    return false;
                }

                if (!command.execute(sender, input.getInput(ARGUMENTS), channel)) {
                    sender.sendMessage(Messages.tl("helpHeader", String.format("%s %s", this.getName(), command.getName())));
                    sender.sendMessage(Messages.tl("helpDescription", command.getDescription()));
                    sender.sendMessage(Messages.tl("helpUsage", command.getUsage()));
                }

                return true;
            }

            sender.sendMessage(Messages.tl("modifyDenied", channel.getFullName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModify(channel)) {
                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), input.get(CHANNEL))) {
                        completion.add(channel.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        if (input.getLength() == COMMAND + 1) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final List<String> completion = new ArrayList<>();

                for (final SubCommand command : this.getCommands()) {
                    if (StringUtil.startsWithIgnoreCase(command.getName(), input.get(COMMAND))) {
                        completion.add(command.getName());
                    }
                }

                Collections.sort(completion);

                return completion;
            }
        }

        if (input.getLength() > ARGUMENTS) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final SubCommand command = this.getCommand(input.get(COMMAND));

                if (command == null) {
                    return Collections.emptyList();
                }

                return command.tabComplete(sender, input.getInput(ARGUMENTS), channel);
            }
        }

        return Collections.emptyList();
    }

    /**
     * The modify remove command that allows to remove a setting from a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static final class RemoveCommand extends SubCommand {

        private static final int TYPE = 0;

        protected RemoveCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "remove", 1, 1);

            this.setDescription("Removes a setting from a channel.");
            this.setUsage("/channel modify <channel> remove <type>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) throws InputException {
            if (this.argsInRange(input.getLength())) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null || !type.isRemovable()) {
                    throw new InvalidTypeException(input.get(TYPE));
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
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            if (input.getLength() == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isRemovable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
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
     * The modify reset command that allows to reset a setting from a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static final class ResetCommand extends SubCommand {

        private static final int TYPE = 0;

        protected ResetCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "reset", 1, 1);

            this.setDescription("Resets a setting from a channel.");
            this.setUsage("/channel modify <channel> reset <type>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) throws InputException {
            if (this.argsInRange(input.getLength())) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null || !type.isResettable()) {
                    throw new InvalidTypeException(input.get(TYPE));
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
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            if (input.getLength() == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isResettable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
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
     * The modify set command that allows to set a setting of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static final class SetCommand extends SubCommand {

        private static final int TYPE = 0;
        private static final int VALUE = 1;

        protected SetCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "set", 2, -1);

            this.setDescription("Sets a setting of a channel.");
            this.setUsage("/channel modify <channel> set <type> <value>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) throws InputException {
            if (this.argsInRange(input.getLength())) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null || !type.isSettable()) {
                    throw new InvalidTypeException(input.get(TYPE));
                }

                if (sender.canModify(channel, type)) {
                    if (type == ModifyType.ANNOUNCE_FORMAT) {
                        try {
                            if (channel.setAnnounceFormat(input.getFormat(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidFormatException("announce", ex);
                        }
                    } else if (type == ModifyType.BROADCAST_FORMAT) {
                        try {
                            if (channel.setBroadcastFormat(input.getFormat(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidFormatException("broadcast", ex);
                        }
                    } else if (type == ModifyType.CHAT_FORMAT) {
                        try {
                            if (channel.setChatFormat(input.getFormat(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getChatFormat()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getChatFormat()));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidFormatException("chat", ex);
                        }
                    }

                    if (input.getLength() != this.getMinArgs()) {
                        return false;
                    }

                    if (type == ModifyType.COLOR) {
                        try {
                            if (channel.setColor(input.getChatColor(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getColor() + channel.getColor().name().toLowerCase()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getColor() + channel.getColor().name().toLowerCase()));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidColorException(ex);
                        }
                    } else if (type == ModifyType.CROSS_WORLD) {
                        if (channel.setCrossWorld(input.getBoolean(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCrossWorld())));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCrossWorld())));
                    } else if (type == ModifyType.CUSTOM_FORMAT) {
                        if (channel.setCustomFormat(input.getBoolean(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCustomFormat())));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), Messages.tlState(channel.isCustomFormat())));
                    } else if (type == ModifyType.DISTANCE) {
                        if (channel.setDistance(input.getInteger(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getDistance()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getDistance()));
                    } else if (type == ModifyType.OWNER) {
                        if (channel.isPersist()) {
                            sender.sendMessage(Messages.tlErr("modifyOwnerPersist", channel.getFullName()));
                            return true;
                        }

                        final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(VALUE));

                        if (target == null || !sender.canSee(target)) {
                            throw new InvalidPlayerException(input.get(VALUE));
                        }

                        if (channel.setOwner(target.getUniqueId())) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), target.getName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), target.getName()));
                    } else if (type == ModifyType.PASSWORD) {
                        if (channel.isDefault()) {
                            sender.sendMessage(Messages.tlErr("modifyPasswordDefault", channel.getFullName()));
                            return true;
                        }

                        try {
                            if (channel.setPassword(input.get(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidPasswordException(input.get(VALUE), ex);
                        }
                    } else if (type == ModifyType.SHORT_NAME) {
                        try {
                            if (channel.setShortName(input.get(VALUE))) {
                                sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(), channel.getShortName()));
                                return true;
                            }

                            sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(), channel.getShortName()));
                        } catch (IllegalArgumentException ex) {
                            throw new InvalidNameException(input.get(VALUE), ex);
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
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            if (input.getLength() == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!type.isSettable()) {
                        continue;
                    }

                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
                            completion.add(type.getIdentifier());
                        }
                    }
                }

                return completion;
            }

            if (input.getLength() == VALUE + 1) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

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

                            if (StringUtil.startsWithIgnoreCase(color.name(), input.get(VALUE))) {
                                completion.add(color.name().toLowerCase());
                            }
                        }
                    } else if (type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT) {
                        if (StringUtil.startsWithIgnoreCase(Boolean.FALSE.toString(), input.get(VALUE))) {
                            completion.add(Boolean.FALSE.toString());
                        }

                        if (StringUtil.startsWithIgnoreCase(Boolean.TRUE.toString(), input.get(VALUE))) {
                            completion.add(Boolean.TRUE.toString());
                        }
                    } else if (type == ModifyType.OWNER) {
                        for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                            if (!sender.canSee(target) || channel.isOwner(target.getUniqueId())) {
                                continue;
                            }

                            if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(VALUE))) {
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
    }
}
