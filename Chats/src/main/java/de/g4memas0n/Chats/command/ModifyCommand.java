package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.channel.IChannel;
import de.g4memas0n.Chats.channel.type.ModifyType;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.util.Permission;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Modify Command, extends {@link ChatsCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 18th, 2020
 * changed: February 3rd, 2020
 */
public final class ModifyCommand extends ChatsCommand {

    private static final String NAME = "modify";
    private static final int MIN_ARGS = 2;
    private static final int MAX_ARGS = 3;

    private static final int ARG_CHANNEL = 0;
    private static final int ARG_MODIFY_TYPE = 1;
    private static final int ARG_VALUE = 2;

    public ModifyCommand() {
        super(NAME, Permission.CHANNEL_MODIFY.getName(), MIN_ARGS, MAX_ARGS);
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] arguments) {
        if (!sender.hasPermission(this.getPermission())) {
            sender.sendMessage(""); //TODO: Add localized 'command_permissionMessage' message.
            return true;
        }

        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getInstance().getChatterManager().getPermissible(sender);
            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(""); //TODO: Add localized 'channel_notExist' message.
                return true;
            }

            final ModifyType modifyType = ModifyType.getType(arguments[ARG_MODIFY_TYPE]);

            if (modifyType == null) {
                return false;
            }

            if (permissible.canModify(channel, modifyType)) {
                boolean success = false;

                try {
                    switch (modifyType) {
                        case ANNOUNCE_FORMAT:
                            success = channel.setAnnounceFormat(arguments[ARG_VALUE]);
                            break;
                        case BROADCAST_FORMAT:
                            success = channel.setBroadcastFormat(arguments[ARG_VALUE]);
                            break;
                        case CHAT_FORMAT:
                            success = channel.setChatFormat(arguments[ARG_VALUE]);
                            break;
                        case COLOR:
                            success = channel.setChatColor(ChatColor.valueOf(arguments[ARG_VALUE]));
                            break;
                        case CROSS_WORLD:
                            success = channel.setCrossWorld(Boolean.getBoolean(arguments[ARG_VALUE]));
                            break;
                        case DISTANCE:
                            success = channel.setDistance(Integer.parseInt(arguments[ARG_VALUE]));
                            break;
                        case PASSWORD:
                            success = channel.setPassword(arguments[ARG_VALUE]);
                            break;
                        case SHORT_NAME:
                            success = channel.setShortName(arguments[ARG_VALUE]);
                            break;
                        case USE_CUSTOM_FORMAT:
                            success = channel.setUseCustomFormat(Boolean.getBoolean(arguments[ARG_VALUE]));
                            break;
                    }
                } catch (IllegalArgumentException ex) {
                    sender.sendMessage(""); //TODO: Add localized 'channel_modifyInvalidValue' message.
                    return true;
                }

                if (success) {
                    sender.sendMessage(""); //TODO: Add localized 'channel_modifySuccess' message.
                    return true;
                }

                sender.sendMessage(""); //TODO: Add localized 'channel_modifyAlready' message.
                return true;
            }

            sender.sendMessage(""); //TODO: Add localized 'channel_modifyDenied' message.
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                               @NotNull final Command command,
                                               @NotNull final String alias,
                                               @NotNull final String[] arguments) {
        final List<String> completion = new ArrayList<>();

        if (!sender.hasPermission(this.getPermission())) {
            return completion;
        }

        if (this.argsInRange(arguments.length)) {
            final IPermissible permissible = this.getInstance().getChatterManager().getPermissible(sender);

            if (arguments.length == ARG_CHANNEL + 1) {
                for (final IChannel current : this.getInstance().getChannelManager().getChannels()) {
                    if (current.isConversation()) {
                        continue;
                    }

                    if (permissible.canModify(current)) {
                        if (current.getFullName().contains(arguments[ARG_CHANNEL])) {
                            completion.add(current.getFullName());
                        }
                    }
                }
            } else if (arguments.length == ARG_MODIFY_TYPE + 1) {
                final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[ARG_CHANNEL]);

                if (channel == null || channel.isConversation()) {
                    return completion;
                }

                for (final ModifyType current : ModifyType.values()) {
                    if (permissible.canModify(channel, current)) {
                        if (current.getIdentifier().contains(arguments[ARG_MODIFY_TYPE])) {
                            completion.add(current.getIdentifier());
                        }
                    }
                }
            }

            Collections.sort(completion);
        }

        return completion;
    }
}
