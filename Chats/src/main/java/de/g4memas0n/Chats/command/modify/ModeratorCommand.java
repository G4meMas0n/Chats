package de.g4memas0n.chats.command.modify;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidChannelException;
import de.g4memas0n.chats.util.input.InvalidPlayerException;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The moderator command that allows to add, list or remove moderators from a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: April 25th, 2020
 * changed: June 22th, 2020
 */
public final class ModeratorCommand extends ModifyingCommand {

    public ModeratorCommand() {
        super("moderator", 3);

        this.addCommand(new AddCommand(this));
        this.addCommand(new ListCommand(this));
        this.addCommand(new RemoveCommand(this));

        this.setDescription("Adds, lists or removes moderators from a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel moderator <channel> (add|list|remove) [<player>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new InvalidChannelException(input.get(CHANNEL));
            }

            if (sender.canModify(channel, ModifyType.MODERATORS)) {
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

            sender.sendMessage(Messages.tl("modifyDeniedType", channel.getFullName(), Messages.tlType(ModifyType.MODERATORS)));
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

                if (sender.canModify(channel, ModifyType.MODERATORS)) {
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

            if (sender.canModify(channel, ModifyType.MODERATORS)) {
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

            if (sender.canModify(channel, ModifyType.MODERATORS)) {
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
     * The moderator add command that allows to add a moderator to a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static class AddCommand extends SubCommand {

        private static final int TARGET = 0;

        protected AddCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "add", 1, 1);

            this.setDescription("Adds a moderator to a channel.");
            this.setUsage("/channel moderator <channel> add <player>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) throws InputException {
            if (this.argsInRange(input.getLength())) {
                final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

                if (target == null || !sender.canSee(target)) {
                    throw new InvalidPlayerException(input.get(TARGET));
                }

                if (channel.setModerator(target.getUniqueId(), true)) {
                    sender.sendMessage(Messages.tl("moderatorAdd", target.getDisplayName(), channel.getFullName()));
                    return true;
                }

                sender.sendMessage(Messages.tl("moderatorAddAlready", target.getDisplayName(), channel.getFullName()));
                return true;
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            if (input.getLength() == TARGET + 1) {
                final List<String> completion = new ArrayList<>();

                for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                    if (channel.isModerator(target.getUniqueId()) || channel.isOwner(target.getUniqueId())) {
                        continue;
                    }

                    if (sender.canSee(target)) {
                        if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(TARGET))) {
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
     * The moderator list command that allows to list the moderators of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static class ListCommand extends SubCommand {

        protected ListCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "list", 0, 0);

            this.setDescription("List moderators of a channel.");
            this.setUsage("/channel moderator <channel> list");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) {
            if (this.argsInRange(input.getLength())) {
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
                    sender.sendMessage(Messages.tl("moderatorNobody", channel.getFullName()));
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
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            return Collections.emptyList();
        }
    }

    /**
     * The moderator remove command that allows to remove a moderator from a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     *
     * created: June 20th, 2020
     * changed: June 22th, 2020
     */
    public static class RemoveCommand extends SubCommand {

        private static final int TARGET = 0;

        protected RemoveCommand(@NotNull final ModifyingCommand parent) {
            super(parent, "remove", 1, 1);

            this.setDescription("Removes a moderator from a channel.");
            this.setUsage("/channel moderator <channel> remove <player>");
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel) throws InputException {
            if (this.argsInRange(input.getLength())) {
                final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(TARGET));

                if (target == null) {
                    throw new InvalidPlayerException(input.get(TARGET));
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

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel) {
            if (input.getLength() == TARGET + 1) {
                final List<String> completion = new ArrayList<>();

                for (final UUID uniqueId : channel.getModerators()) {
                    final IOfflineChatter moderator = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (moderator == null) {
                        channel.setModerator(uniqueId, false);
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(moderator.getName(), input.get(TARGET))) {
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
