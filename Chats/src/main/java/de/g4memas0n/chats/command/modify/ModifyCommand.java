package de.g4memas0n.chats.command.modify;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidArgumentException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.ChatColor;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * The modify command that allows to modify a channel.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 */
public final class ModifyCommand extends BasicCommand {

    private static final int CHANNEL = 0;
    private static final int TYPE = 1;
    private static final int MODIFIER = 2;
    private static final int VALUE = 3;

    private final Set<Modifier> modifiers;

    public ModifyCommand() {
        super("modify", 2, -1);

        this.setDescription("Modifies a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel modify <channel> <type> <modifier> [<value>]");

        this.modifiers = new HashSet<>(6, 1);

        this.modifiers.add(new ModeratorAddModifier(this));
        this.modifiers.add(new ModeratorRemoveModifier(this));
        this.modifiers.add(new RemoveModifier(this));
        this.modifiers.add(new ResetModifier(this));
        this.modifiers.add(new SetModifier(this));
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                throw new ChannelNotExistException(input.get(CHANNEL));
            }

            if (sender.canModify(channel)) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null) {
                    throw new InvalidArgumentException("invalidType", input.get(TYPE));
                }

                if (sender.canModify(channel, type)) {
                    for (final Modifier modifier : this.modifiers) {
                        if (!modifier.accept(type)) {
                            continue;
                        }

                        if (modifier.getIdentifier().equalsIgnoreCase(input.get(MODIFIER))) {
                            return modifier.execute(sender, input, channel, type);
                        }
                    }

                    return false;
                }

                sender.sendMessage(Messages.tl("modifyDeniedType", Messages.tlType(type), channel.getFullName()));
                return true;
            }

            sender.sendMessage(Messages.tl("modifyDenied", channel.getFullName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> help(@NotNull final ICommandSource sender,
                                      @NotNull final ICommandInput input) {
        if (input.getLength() >= MODIFIER + 1) {
            final ModifyType type = ModifyType.getType(input.get(TYPE));

            if (type != null) {
                for (final Modifier modifier : this.modifiers) {
                    if (!modifier.accept(type)) {
                        continue;
                    }

                    if (modifier.getIdentifier().equalsIgnoreCase(input.get(MODIFIER))) {
                        final List<String> help = new ArrayList<>();

                        help.add(Messages.tl("helpHeader", String.format("modify <channel> %s %s",
                                type.getIdentifier(), modifier.getIdentifier())));
                        help.add(Messages.tl("helpDescription", modifier.getDescription()));
                        help.add(Messages.tl("helpUsage", modifier.getUsage(type)));

                        return help;
                    }
                }

                final List<String> help = new ArrayList<>();

                help.add(Messages.tl("helpHeader", String.format("modify <channel> %s", type.getIdentifier())));
                help.add(Messages.tl("helpDescription", this.getDescription()));
                help.add(Messages.tl("helpUsage", this.getUsage(type)));
                help.add(Messages.tlJoin("helpModifiers", this.modifiers.stream()
                        .filter(modifier -> modifier.accept(type))
                        .map(Modifier::getIdentifier)
                        .collect(Collectors.toList())));

                return help;
            }
        }

        final List<String> help = super.help(sender, input);

        help.add(Messages.tlJoin("helpTypes", Arrays.stream(ModifyType.values())
                .map(ModifyType::getIdentifier)
                .collect(Collectors.toList())));

        return help;
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

        if (input.getLength() == TYPE + 1) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (sender.canModify(channel, type)) {
                        if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), input.get(TYPE))) {
                            completion.add(type.getIdentifier());
                        }
                    }
                }

                return completion;
            }
        }

        if (input.getLength() == MODIFIER + 1) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null) {
                    return Collections.emptyList();
                }

                if (sender.canModify(channel, type)) {
                    final List<String> completion = new ArrayList<>();

                    for (final Modifier modifier : this.modifiers) {
                        if (!modifier.accept(type)) {
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(modifier.getIdentifier(), input.get(MODIFIER))) {
                            completion.add(modifier.getIdentifier());
                        }
                    }

                    return completion;
                }
            }
        }

        if (input.getLength() > MODIFIER + 1) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null || channel.isConversation()) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel)) {
                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null) {
                    return Collections.emptyList();
                }

                if (sender.canModify(channel, type)) {
                    for (final Modifier modifier : this.modifiers) {
                        if (!modifier.accept(type)) {
                            continue;
                        }

                        if (modifier.getIdentifier().equalsIgnoreCase(input.get(MODIFIER))) {
                            return modifier.tabComplete(sender, input, channel, type);
                        }
                    }
                }
            }
        }

        return Collections.emptyList();
    }

    public @NotNull String getUsage(@NotNull final ModifyType type) {
        return this.getUsage().replace("<type>", type.getIdentifier());
    }

    /**
     * The abstract modifier representation, for modifying channel settings.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public abstract static class Modifier {

        private final BasicCommand command;

        private final String identifier;
        private final int minArgs;
        private final int maxArgs;

        private String description;
        private String usage;

        protected Modifier(@NotNull final BasicCommand command,
                           @NotNull final String identifier,
                           final int minArgs,
                           final int maxArgs) {
            this.command = command;
            this.identifier = identifier;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        public final @NotNull IChats getInstance() {
            return this.command.getInstance();
        }

        public final @NotNull String getIdentifier() {
            return this.identifier;
        }

        public final int getMinArgs() {
            return this.minArgs;
        }

        public final int getMaxArgs() {
            return this.maxArgs;
        }

        /**
         * Returns whether this modifier accepts the given modify type.
         *
         * @param type the modify type to check.
         * @return true when this modifier accepts the given type.
         */
        public abstract boolean accept(@NotNull final ModifyType type);

        /**
         * Executes the modifier for the given sender, channel and type, returning its success.
         *
         * <p>If false is returned, then the help of the modifier will be sent to the sender.</p>
         *
         * @param sender the source who executed the modifier.
         * @param input the input of the sender, including passed arguments.
         * @param channel the channel to modify.
         * @param type the type to modify.
         * @return true if the modifier execution was valid, false otherwise.
         * @throws InputException Thrown when an argument of the input is invalid.
         */
        public abstract boolean execute(@NotNull final ICommandSource sender,
                                        @NotNull final ICommandInput input,
                                        @NotNull final IChannel channel,
                                        @NotNull final ModifyType type) throws InputException;

        /**
         * Requests a list of possible completions for a modifier argument.
         *
         * @param sender the source who tab-completed the modifier.
         * @param input the input of the sender, including the passed arguments including the final partial argument to
         *              be completed.
         * @param channel the channel to modify.
         * @param type the type to modify.
         * @return a list of possible completions for the final arguments.
         */
        public abstract @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                          @NotNull final ICommandInput input,
                                                          @NotNull final IChannel channel,
                                                          @NotNull final ModifyType type);

        public final @NotNull String getDescription() {
            return this.description;
        }

        public final void setDescription(@NotNull final String description) {
            if (description.equals(this.description)) {
                return;
            }

            this.description = description;
        }

        public final @NotNull String getUsage(@NotNull final ModifyType type) {
            return this.usage.replace("<type>", type.getIdentifier());
        }

        public final void setUsage(@NotNull final String usage) {
            if (usage.equals(this.usage)) {
                return;
            }

            this.usage = usage;
        }
    }

    /**
     * The moderator add modifier that allows to add moderators to a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class ModeratorAddModifier extends Modifier {

        protected ModeratorAddModifier(@NotNull final BasicCommand command) {
            super(command, "add", 4, 4);

            this.setDescription("Adds a moderator to a channel.");
            this.setUsage("/channel modify <channel> moderator add <player>");
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type == ModifyType.MODERATOR;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) throws InputException {
            if (input.getLength() == this.getMaxArgs() && type == ModifyType.MODERATOR) {
                final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(VALUE));

                if (target == null || !sender.canSee(target)) {
                    throw new PlayerNotFoundException(input.get(VALUE));
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
                                                 @NotNull final IChannel channel,
                                                 @NotNull final ModifyType type) {
            if (input.getLength() == VALUE + 1 && type == ModifyType.MODERATOR) {
                final List<String> completion = new ArrayList<>();

                for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                    if (channel.isModerator(target.getUniqueId()) || channel.isOwner(target.getUniqueId())) {
                        continue;
                    }

                    if (sender.canSee(target)) {
                        if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(VALUE))) {
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
     * The moderator remove modifier that allows to remove moderators from a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class ModeratorRemoveModifier extends Modifier {

        protected ModeratorRemoveModifier(@NotNull final BasicCommand command) {
            super(command, "remove", 4, 4);

            this.setDescription("Removes a moderator from a channel.");
            this.setUsage("/channel modify <channel> moderator remove <player>");
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type == ModifyType.MODERATOR;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) throws InputException {
            if (input.getLength() == this.getMaxArgs() && type == ModifyType.MODERATOR) {
                final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(input.get(VALUE));

                if (target == null) {
                    throw new PlayerNotFoundException(input.get(VALUE));
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
                                                 @NotNull final IChannel channel,
                                                 @NotNull final ModifyType type) {
            if (input.getLength() == VALUE + 1 && type == ModifyType.MODERATOR) {
                final List<String> completion = new ArrayList<>();

                for (final UUID uniqueId : channel.getModerators()) {
                    final IOfflineChatter moderator = this.getInstance().getChatterManager().getOfflineChatter(uniqueId);

                    if (moderator == null) {
                        channel.setModerator(uniqueId, false);
                        continue;
                    }

                    if (StringUtil.startsWithIgnoreCase(moderator.getName(), input.get(VALUE))) {
                        completion.add(moderator.getName());
                    }
                }

                Collections.sort(completion);

                return completion;
            }

            return Collections.emptyList();
        }
    }

    /**
     * The modify remove modifier that allows to remove settings of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class RemoveModifier extends Modifier {

        protected RemoveModifier(@NotNull final BasicCommand command) {
            super(command, "remove", 3, 4);

            this.setDescription("Removes a setting from a channel.");
            this.setUsage("/channel modify <channel> <type> remove");
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type != ModifyType.COLOR && type != ModifyType.CROSS_WORLD
                    && type != ModifyType.CUSTOM_FORMAT && type != ModifyType.MODERATOR;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) {
            if (input.getLength() == this.getMinArgs()) {
                if (type == ModifyType.ANNOUNCE_FORMAT) {
                    if (channel.setAnnounceFormat(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.BROADCAST_FORMAT) {
                    if (channel.setBroadcastFormat(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.CHAT_FORMAT) {
                    if (channel.setChatFormat(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.DISTANCE) {
                    if (channel.setDistance(-1)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.OWNER) {
                    if (channel.setOwner(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.PASSWORD) {
                    if (channel.setPassword(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.SHORT_NAME) {
                    if (channel.setShortName(null)) {
                        sender.sendMessage(Messages.tl("modifyRemove", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotSet", Messages.tlType(type), channel.getFullName()));
                    return true;
                }
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final ModifyType type) {
            return Collections.emptyList();
        }
    }

    /**
     * The modify reset modifier that allows to reset settings of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class ResetModifier extends Modifier {

        protected ResetModifier(@NotNull final BasicCommand command) {
            super(command, "reset", 3, 3);

            this.setDescription("Resets a setting from a channel.");
            this.setUsage("/channel modify <channel> <type> reset");
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type == ModifyType.COLOR || type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) {
            if (input.getLength() == this.getMaxArgs()) {
                if (type == ModifyType.COLOR) {
                    if (channel.setColor(null)) {
                        sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.CROSS_WORLD) {
                    if (channel.setCrossWorld(true)) {
                        sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    return true;
                } else if (type == ModifyType.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(true)) {
                        sender.sendMessage(Messages.tl("modifyReset", Messages.tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyNotChanged", Messages.tlType(type), channel.getFullName()));
                    return true;
                }
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final ModifyType type) {
            return Collections.emptyList();
        }
    }

    /**
     * The modify set modifier that allows to set settings of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class SetModifier extends Modifier {

        protected SetModifier(@NotNull final BasicCommand command) {
            super(command, "set", 4, -1);

            this.setDescription("Sets a setting of a channel.");
            this.setUsage("/channel modify <channel> <type> set <value>");
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type != ModifyType.MODERATOR;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) throws InputException {
            if (input.getLength() >= this.getMinArgs()) {
                if (type == ModifyType.ANNOUNCE_FORMAT) {
                    try {
                        if (channel.setAnnounceFormat(input.getFormat(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type),
                                    channel.getFullName(), channel.getAnnounceFormat()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type),
                                channel.getFullName(), channel.getAnnounceFormat()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", Messages.tl("announce"));
                    }
                } else if (type == ModifyType.BROADCAST_FORMAT) {
                    try {
                        if (channel.setBroadcastFormat(input.getFormat(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type),
                                    channel.getFullName(), channel.getBroadcastFormat()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type),
                                channel.getFullName(), channel.getBroadcastFormat()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", Messages.tl("broadcast"));
                    }
                } else if (type == ModifyType.CHAT_FORMAT) {
                    try {
                        if (channel.setChatFormat(input.getFormat(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type),
                                    channel.getFullName(), channel.getChatFormat()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type),
                                channel.getFullName(), channel.getChatFormat()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", Messages.tl("chat"));
                    }
                }
            }

            if (input.getLength() == this.getMinArgs()) {
                if (type == ModifyType.COLOR) {
                    try {
                        if (channel.setColor(input.getChatColor(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                    channel.getColor() + channel.getColor().name().toLowerCase()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                                channel.getColor() + channel.getColor().name().toLowerCase()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidColor", input.get(VALUE));
                    }
                } else if (type == ModifyType.CROSS_WORLD) {
                    if (channel.setCrossWorld(input.getBoolean(VALUE))) {
                        sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                Messages.tlState(channel.isCrossWorld())));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                            Messages.tlState(channel.isCrossWorld())));
                    return true;
                } else if (type == ModifyType.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(input.getBoolean(VALUE))) {
                        sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                Messages.tlState(channel.isCustomFormat())));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                            Messages.tlState(channel.isCustomFormat())));
                    return true;
                } else if (type == ModifyType.DISTANCE) {
                    if (channel.setDistance(input.getInteger(VALUE))) {
                        sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                channel.getDistance()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                            channel.getDistance()));
                    return true;
                } else if (type == ModifyType.OWNER) {
                    if (channel.isPersist()) {
                        sender.sendMessage(Messages.tlErr("modifyOwnerPersist", channel.getFullName()));
                        return true;
                    }

                    final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(VALUE));

                    if (target == null || !sender.canSee(target)) {
                        throw new PlayerNotFoundException(input.get(VALUE));
                    }

                    if (channel.setOwner(target.getUniqueId())) {
                        sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                target.getName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                            target.getName()));
                    return true;
                } else if (type == ModifyType.PASSWORD) {
                    if (channel.isDefault()) {
                        sender.sendMessage(Messages.tlErr("modifyPasswordDefault", channel.getFullName()));
                        return true;
                    }

                    try {
                        if (channel.setPassword(input.get(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type), channel.getFullName(),
                                    channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type), channel.getFullName(),
                                channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidPassword", input.get(VALUE));
                    }
                } else if (type == ModifyType.SHORT_NAME) {
                    try {
                        if (channel.setShortName(input.get(VALUE))) {
                            sender.sendMessage(Messages.tl("modifySet", Messages.tlType(type),
                                    channel.getFullName(), channel.getShortName()));
                            return true;
                        }

                        sender.sendMessage(Messages.tl("modifyAlready", Messages.tlType(type),
                                channel.getFullName(), channel.getShortName()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidName", input.get(VALUE));
                    }
                }
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final ModifyType type) {
            if (input.getLength() == VALUE + 1) {
                if (type == ModifyType.COLOR) {
                    final List<String> completion = new ArrayList<>();

                    for (final ChatColor color : ChatColor.values()) {
                        if (!color.isColor()) {
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(color.name(), input.get(VALUE))) {
                            completion.add(color.name().toLowerCase());
                        }
                    }

                    return completion;
                } else if (type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT) {
                    final List<String> completion = new ArrayList<>();

                    if (StringUtil.startsWithIgnoreCase(Boolean.FALSE.toString(), input.get(VALUE))) {
                        completion.add(Boolean.FALSE.toString());
                    }

                    if (StringUtil.startsWithIgnoreCase(Boolean.TRUE.toString(), input.get(VALUE))) {
                        completion.add(Boolean.TRUE.toString());
                    }

                    return completion;
                } else if (type == ModifyType.OWNER) {
                    final List<String> completion = new ArrayList<>();

                    for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                        if (!sender.canSee(target) || channel.isOwner(target.getUniqueId())) {
                            continue;
                        }

                        if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(VALUE))) {
                            completion.add(target.getName());
                        }
                    }

                    Collections.sort(completion);

                    return completion;
                }
            }

            return Collections.emptyList();
        }
    }
}
