package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.util.Permission;
import de.g4memas0n.Chats.util.type.ModifyType;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.util.InputUtil;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Modify Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 18th, 2020
 * changed: March 3rd, 2020
 */
public final class ModifyCommand extends BasicCommand {

    private static final String NAME = "modify";
    private static final int MIN_ARGS = 3;
    private static final int MAX_ARGS = 4;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_TYPE = 1;
    private static final int ARG_COMMAND = 2;
    private static final int ARG_VALUE = 3;

    private static final String SET = "set";
    private static final String REMOVE = "remove";
    private static final String RESET = "reset";

    public ModifyCommand() {
        super(NAME, Permission.CHANNEL_MODIFY.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean execute(@NotNull final CommandSender sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getPermissible(sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[ARG_CHANNEL]));
                return true;
            }

            final ModifyType modifyType = ModifyType.getType(arguments[ARG_TYPE]);

            if (modifyType == null) {
                return false;
            }

            if (permissible.canModify(channel, modifyType)) {
                switch (modifyType) {
                    case ANNOUNCE_FORMAT:
                        return this.modifyAnnounceFormat(sender, channel, arguments);

                    case BROADCAST_FORMAT:
                        return this.modifyBroadcastFormat(sender, channel, arguments);

                    case CHAT_FORMAT:
                        return this.modifyChatFormat(sender, channel, arguments);

                    case COLOR:
                        return this.modifyColor(sender, channel, arguments);

                    case CROSS_WORLD:
                        return this.modifyCrossWorld(sender, channel, arguments);

                    case CUSTOM_FORMAT:
                        return this.modifyCustomFormat(sender, channel, arguments);

                    case DISTANCE:
                        return this.modifyDistance(sender, channel, arguments);

                    case PASSWORD:
                        return this.modifyPassword(sender, channel, arguments);

                    case SHORT_NAME:
                        return this.modifyShortName(sender, channel, arguments);

                    default:
                        return false;
                }
            }

            sender.sendMessage(Messages.tl("modifyDenied", modifyType.getIdentifier(), channel.getFullName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final List<String> completion = new ArrayList<>();
            final IPermissible permissible = this.getPermissible(sender);

            if (arguments.length == ARG_CHANNEL + 1) {
                for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                    if (current.isConversation()) {
                        continue;
                    }

                    if (InputUtil.containsInput(current.getFullName(), arguments[ARG_CHANNEL])) {
                        if (permissible.canModify(current)) {
                            completion.add(current.getFullName());
                        }
                    }
                }
            } else {
                final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

                if (channel == null || channel.isConversation()) {
                    return Collections.emptyList();
                }

                if (arguments.length == ARG_TYPE + 1) {
                    for (final ModifyType current : ModifyType.values()) {
                        if (InputUtil.containsInput(current.getIdentifier(), arguments[ARG_TYPE])) {
                            if (permissible.canModify(channel, current)) {
                                completion.add(current.getIdentifier());
                            }
                        }
                    }
                } else {
                    final ModifyType type = ModifyType.getType(arguments[ARG_TYPE]);

                    if (type == null) {
                        return Collections.emptyList();
                    }

                    if (arguments.length == this.getMaxArgs()) {
                        if (type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT) {
                            if (InputUtil.containsInput(InputUtil.BOOLEAN_FALSE, arguments[ARG_VALUE])) {
                                completion.add(InputUtil.BOOLEAN_FALSE);
                            }

                            if (InputUtil.containsInput(InputUtil.BOOLEAN_TRUE, arguments[ARG_VALUE])) {
                                completion.add(InputUtil.BOOLEAN_TRUE);
                            }
                        } else if (type == ModifyType.COLOR) {
                            for (final ChatColor current : ChatColor.values()) {
                                if (!current.isColor()) {
                                    continue;
                                }

                                if (InputUtil.containsInput(current.name(), arguments[ARG_VALUE])) {
                                    completion.add(current.name());
                                }
                            }
                        }
                    } else {
                        if (type.isAdjustable() && InputUtil.containsInput(SET, arguments[ARG_COMMAND])) {
                            completion.add(SET);
                        }

                        if (type.isRemovable()) {
                            if (InputUtil.containsInput(REMOVE, arguments[ARG_COMMAND])) {
                                completion.add(REMOVE);
                            }
                        } else {
                            if (InputUtil.containsInput(RESET, arguments[ARG_COMMAND])) {
                                completion.add(RESET);
                            }
                        }
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }

    private boolean modifyAnnounceFormat(@NotNull final CommandSender sender,
                                         @NotNull final IChannel channel,
                                         @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setAnnounceFormat(null);
                sender.sendMessage(Messages.tl("modifyFormatRemoved", Messages.tl("announce"),
                        channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setAnnounceFormat(arguments[ARG_VALUE])) {
                        sender.sendMessage(Messages.tl("modifyFormatChanged", Messages.tl("announce"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    } else {
                        sender.sendMessage(Messages.tl("modifyFormatAlready", Messages.tl("announce"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidFormat"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyBroadcastFormat(@NotNull final CommandSender sender,
                                          @NotNull final IChannel channel,
                                          @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setBroadcastFormat(null);
                sender.sendMessage(Messages.tl("modifyFormatRemoved", Messages.tl("broadcast"),
                        channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setBroadcastFormat(arguments[ARG_VALUE])) {
                        sender.sendMessage(Messages.tl("modifyFormatChanged", Messages.tl("broadcast"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    } else {
                        sender.sendMessage(Messages.tl("modifyFormatAlready", Messages.tl("broadcast"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidFormat"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyChatFormat(@NotNull final CommandSender sender,
                                     @NotNull final IChannel channel,
                                     @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setChatFormat(null);
                sender.sendMessage(Messages.tl("modifyFormatRemoved", Messages.tl("chat"),
                        channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setChatFormat(arguments[ARG_VALUE])) {
                        sender.sendMessage(Messages.tl("modifyFormatChanged", Messages.tl("chat"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    } else {
                        sender.sendMessage(Messages.tl("modifyFormatAlready", Messages.tl("chat"),
                                channel.getColoredName(), arguments[ARG_VALUE]));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidFormat"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyColor(@NotNull final CommandSender sender,
                                @NotNull final IChannel channel,
                                @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(RESET)) {
                channel.setChatColor(null);
                sender.sendMessage(Messages.tl("modifyColorReset", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setChatColor(InputUtil.parseChatColor(arguments[ARG_VALUE]))) {
                        sender.sendMessage(Messages.tl("modifyColorChanged", channel.getColoredName(),
                                this.getColoredColor(channel.getChatColor())));
                    } else {
                        sender.sendMessage(Messages.tl("modifyColorAlready", channel.getColoredName(),
                                this.getColoredColor(channel.getChatColor())));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidColor"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyCrossWorld(@NotNull final CommandSender sender,
                                     @NotNull final IChannel channel,
                                     @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(RESET)) {
                channel.setCrossWorld(true);
                sender.sendMessage(Messages.tl("modifyCrossWorldReset", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setCrossWorld(InputUtil.parseBoolean(arguments[ARG_VALUE]))) {
                        sender.sendMessage(Messages.tl("modifyCrossWorldChanged", channel.getColoredName(),
                                channel.isCrossWorld() ? Messages.tl("enabled") : Messages.tl("disabled")));
                    } else {
                        sender.sendMessage(Messages.tl("modifyCrossWorldAlready", channel.getColoredName(),
                                channel.isCrossWorld() ? Messages.tl("enabled") : Messages.tl("disabled")));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidBoolean"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyCustomFormat(@NotNull final CommandSender sender,
                                       @NotNull final IChannel channel,
                                       @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(RESET)) {
                channel.setCustomFormat(false);
                sender.sendMessage(Messages.tl("modifyCustomFormatReset", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setCustomFormat(InputUtil.parseBoolean(arguments[ARG_VALUE]))) {
                        sender.sendMessage(Messages.tl("modifyCustomFormatChanged", channel.getFullName(),
                                channel.isCustomFormat() ? Messages.tl("enabled") : Messages.tl("disabled")));
                    } else {
                        sender.sendMessage(Messages.tl("modifyCustomFormatAlready", channel.getFullName(),
                                channel.isCustomFormat() ? Messages.tl("enabled") : Messages.tl("disabled")));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidBoolean"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyDistance(@NotNull final CommandSender sender,
                                   @NotNull final IChannel channel,
                                   @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setDistance(-1);
                sender.sendMessage(Messages.tl("modifyDistanceRemoved", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setDistance(InputUtil.parseInt(arguments[ARG_VALUE]))) {
                        sender.sendMessage(Messages.tl("modifyDistanceChanged", channel.getColoredName(),
                                channel.getDistance()));
                    } else {
                        sender.sendMessage(Messages.tl("modifyDistanceAlready", channel.getColoredName(),
                                channel.getDistance()));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidNumber"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyPassword(@NotNull final CommandSender sender,
                                   @NotNull final IChannel channel,
                                   @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setPassword(null);
                sender.sendMessage(Messages.tl("modifyPasswordRemoved", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setPassword(arguments[ARG_VALUE])) {
                        sender.sendMessage(Messages.tl("modifyPasswordChanged", channel.getColoredName(),
                                arguments[ARG_VALUE]));
                    } else {
                        sender.sendMessage(Messages.tl("modifyPasswordAlready", channel.getColoredName(),
                                arguments[ARG_VALUE]));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidPassword"));
                }

                return true;
            }
        }

        return false;
    }

    private boolean modifyShortName(@NotNull final CommandSender sender,
                                    @NotNull final IChannel channel,
                                    @NotNull final String[] arguments) {
        if (arguments.length == this.getMinArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(REMOVE)) {
                channel.setShortName(null);
                sender.sendMessage(Messages.tl("modifyShortNameRemoved", channel.getColoredName()));
                return true;
            }
        } else if (arguments.length == this.getMaxArgs()) {
            if (arguments[ARG_COMMAND].equalsIgnoreCase(SET)) {
                try {
                    if (channel.setShortName(arguments[ARG_VALUE])) {
                        sender.sendMessage(Messages.tl("modifyShortNameChanged", channel.getColoredName(),
                                arguments[ARG_VALUE]));
                    } else {
                        sender.sendMessage(Messages.tl("modifyShortNameAlready", channel.getColoredName(),
                                arguments[ARG_VALUE]));
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(Messages.tlErr("invalidName"));
                }

                return true;
            }
        }

        return false;
    }

    private @NotNull String getColoredColor(@NotNull final ChatColor color) {
        return color.toString() + color.name();
    }
}
