package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Moderator Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: April 25th, 2020
 * changed: June 20th, 2020
 */
public final class ModeratorCommand extends ModifyingCommand {

    public ModeratorCommand() {
        super("moderator", 4, 1);

        this.addCommand(new AddCommand(this));
        this.addCommand(new ListCommand(this));
        this.addCommand(new RemoveCommand(this));

        this.setDescription("Adds, lists or removes moderators from a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel moderator <channel> (add|list|remove) [<player>]");
    }

    /**
     * The moderator add command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static class AddCommand extends ModifyingSubCommand {

        private static final int TARGET = 0;

        protected AddCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "add", 1, 1);

            this.setDescription("Adds a moderator to a channel.");
            this.setUsage("/channel moderator <channel> add <player>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final ModifyType type = ModifyType.MODERATORS;

                if (sender.canModify(channel, type)) {
                    final IChatter target = this.getInstance().getChatterManager().getChatter(arguments[TARGET]);

                    if (target == null || !sender.canSee(target)) {
                        sender.sendMessage(Messages.tlErr("playerNotFound", arguments[TARGET]));
                        return true;
                    }

                    if (channel.setModerator(target.getUniqueId(), true)) {
                        sender.sendMessage(Messages.tl("moderatorAdd", target.getDisplayName(), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("moderatorAddAlready", target.getDisplayName(), channel.getFullName()));
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
            if (arguments.length == TARGET + 1) {
                final List<String> completion = new ArrayList<>();

                for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                    if (channel.isModerator(target.getUniqueId()) || channel.isOwner(target.getUniqueId())) {
                        continue;
                    }

                    if (sender.canSee(target)) {
                        if (StringUtil.startsWithIgnoreCase(target.getName(), arguments[TARGET])) {
                            completion.add(target.getName());
                        }
                    }
                }

                Collections.sort(completion);

                return completion;
            }

            return Collections.emptyList();
        }
    }

    /**
     * The moderator list command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static class ListCommand extends ModifyingSubCommand {

        protected ListCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "list", 0, 0);

            this.setDescription("List moderators of a channel.");
            this.setUsage("/channel moderator <channel> list");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final List<String> moderators = new ArrayList<>();

                for (final UUID uniqueId : channel.getModerators()) {
                    final IOfflineChatter moderator = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (moderator == null) {
                        channel.setModerator(uniqueId, false);
                        continue;
                    }

                    moderators.add(moderator.getName());
                }

                if (moderators.isEmpty()) {
                    sender.sendMessage(Messages.tl("moderatorEmpty", channel.getFullName()));
                    return true;
                }

                Collections.sort(moderators);

                sender.sendMessage(Messages.tl("moderatorHeader", channel.getColoredName()));
                sender.sendMessage(Messages.tlJoin("moderatorList", moderators));
                return true;
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final String[] arguments) {
            return Collections.emptyList();
        }
    }

    /**
     * The moderator remove command, extends {@link ModifyingSubCommand}.
     *
     * @author G4meMas0n
     * @since 0.2.4-SNAPSHOT
     *
     * created: June 20th, 2020
     * changed: June 20th, 2020
     */
    public static class RemoveCommand extends ModifyingSubCommand {

        private static final int TARGET = 0;

        protected RemoveCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "remove", 1, 1);

            this.setDescription("Removes a moderator from a channel.");
            this.setUsage("/channel moderator <channel> remove <player>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final IChannel channel,
                               @NotNull final String[] arguments) {
            if (this.argsInRange(arguments.length)) {
                final ModifyType type = ModifyType.MODERATORS;

                if (sender.canModify(channel, type)) {
                    final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(arguments[TARGET]);

                    if (target == null) {
                        sender.sendMessage(Messages.tlErr("playerNotFound", arguments[TARGET]));
                        return true;
                    }

                    final IChatter online = target instanceof IChatter ? (IChatter) target : null;

                    if (channel.setModerator(target.getUniqueId(), false)) {
                        sender.sendMessage(Messages.tl("moderatorRemove", (online != null && sender.canSee(online))
                                ? online.getDisplayName() : target.getName(), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("moderatorRemoveAlready", (online != null && sender.canSee(online))
                            ? online.getDisplayName() : target.getName(), channel.getFullName()));
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
            if (arguments.length == TARGET + 1) {
                final List<String> completion = new ArrayList<>();

                for (final UUID uniqueId : channel.getModerators()) {
                    final IOfflineChatter moderator = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (moderator == null) {
                        channel.setModerator(uniqueId, false);
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(moderator.getName(), arguments[TARGET])) {
                        completion.add(moderator.getName());
                    }
                }

                Collections.sort(completion);

                return completion;
            }

            return Collections.emptyList();
        }
    }
}
