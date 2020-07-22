package de.g4memas0n.chats.command.modify;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.permission.Permission;
import de.g4memas0n.chats.util.type.ModifyType;
import org.bukkit.ChatColor;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlErr;
import static de.g4memas0n.chats.messaging.Messages.tlState;
import static de.g4memas0n.chats.messaging.Messages.tlType;

/**
 * The modify command that allows to modify a channel.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 */
public final class ModifyCommand extends BasicCommand {

    private static final int CHANNEL = 0;
    private static final int MODIFIER = 1;
    private static final int TYPE = 2;

    private final Set<Modifier> modifiers;

    public ModifyCommand() {
        super("modify", 2, -1);

        this.setDescription("Sets, removes and/or resets settings of a channel.");
        this.setPermission(Permission.MODIFY.getNode());
        this.setUsage("/channel modify <channel> (set|remove|reset) <type> [<value>]");

        this.modifiers = new HashSet<>(4, 1);

        this.modifiers.add(new RemoveModifier(this));
        this.modifiers.add(new ResetModifier(this));
        this.modifiers.add(new SetModifier(this));
    }

    public @Nullable Modifier getModifier(@NotNull final String identifier) {
        for (final Modifier modifier : this.modifiers) {
            if (modifier.getIdentifier().equalsIgnoreCase(identifier)) {
                return modifier;
            }
        }

        return null;
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModify(channel)) {
                    return false;
                }
            }

            return true;
        }

        return false;
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
                final Modifier modifier = this.getModifier(input.get(MODIFIER));

                if (modifier == null) {
                    return false;
                }

                final ModifyType type = ModifyType.getType(input.get(TYPE));

                if (type == null || !modifier.accept(type)) {
                    throw new InvalidArgumentException("invalidType", input.get(TYPE));
                }

                if (sender.canModify(channel, type)) {
                    return modifier.execute(sender, input, channel, type);
                }

                sender.sendMessage(tl("modifyDeniedType", tlType(type), channel.getFullName()));
                return true;
            }

            sender.sendMessage(tl("modifyDenied", channel.getFullName()));
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

        // input.getLength() > 1, because the given input is always >= 1
        final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

        if (channel == null || channel.isConversation()) {
            return Collections.emptyList();
        }

        if (sender.canModify(channel)) {
            if (input.getLength() == MODIFIER + 1) {
                final List<String> completion = new ArrayList<>();

                for (final Modifier modifier : this.modifiers) {
                    if (StringUtil.startsWithIgnoreCase(modifier.getIdentifier(), input.get(MODIFIER))) {
                        completion.add(modifier.getIdentifier());
                    }
                }

                return completion;
            }

            // input.getLength() > 2
            final Modifier modifier = this.getModifier(input.get(MODIFIER));

            if (modifier == null) {
                return Collections.emptyList();
            }

            if (input.getLength() == TYPE + 1) {
                final List<String> completion = new ArrayList<>();

                for (final ModifyType type : ModifyType.values()) {
                    if (!modifier.accept(type)) {
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

            // input.getLength() > 3
            final ModifyType type = ModifyType.getType(input.get(TYPE));

            if (type == null || !modifier.accept(type)) {
                return Collections.emptyList();
            }

            if (sender.canModify(channel, type)) {
                return modifier.tabComplete(sender, input, channel, type);
            }
        }

        return Collections.emptyList();
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

        protected Modifier(@NotNull final BasicCommand command,
                           @NotNull final String identifier,
                           final int minArgs,
                           final int maxArgs) {
            this.command = command;
            this.identifier = identifier;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        public final @NotNull Chats getInstance() {
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
    }

    /**
     * The modify remove modifier that allows to remove settings of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class RemoveModifier extends Modifier {

        protected RemoveModifier(@NotNull final BasicCommand command) {
            super(command, "remove", 3, 3);
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type != ModifyType.COLOR && type != ModifyType.CROSS_WORLD
                    && type != ModifyType.CUSTOM_FORMAT && type != ModifyType.VERBOSE;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) throws InputException {
            if (input.getLength() == this.getMinArgs()) {
                if (type == ModifyType.ANNOUNCE_FORMAT) {
                    if (channel.setAnnounceFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.BROADCAST_FORMAT) {
                    if (channel.setBroadcastFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.CHAT_FORMAT) {
                    if (channel.setChatFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.DISTANCE) {
                    if (channel.setDistance(-1)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.OWNER) {
                    if (channel.setOwner(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.PASSWORD) {
                    if (channel.setPassword(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.SHORT_NAME) {
                    if (channel.setShortName(null)) {
                        sender.sendMessage(tl("modifyRemove", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                throw new InvalidArgumentException("invalidType", type.getIdentifier());
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
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return type == ModifyType.COLOR || type == ModifyType.CROSS_WORLD
                    || type == ModifyType.CUSTOM_FORMAT || type == ModifyType.VERBOSE;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final ModifyType type) throws InputException {
            if (input.getLength() == this.getMaxArgs()) {
                if (type == ModifyType.COLOR) {
                    if (channel.setColor(null)) {
                        sender.sendMessage(tl("modifyReset", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.CROSS_WORLD) {
                    if (channel.setCrossWorld(true)) {
                        sender.sendMessage(tl("modifyReset", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(false)) {
                        sender.sendMessage(tl("modifyReset", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                if (type == ModifyType.VERBOSE) {
                    if (channel.setVerbose(true)) {
                        sender.sendMessage(tl("modifyReset", tlType(type), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tlType(type), channel.getFullName()));
                    return true;
                }

                throw new InvalidArgumentException("invalidType", type.getIdentifier());
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

        private static final int VALUE = 3;

        protected SetModifier(@NotNull final BasicCommand command) {
            super(command, "set", 4, -1);
        }

        @Override
        public boolean accept(@NotNull final ModifyType type) {
            return true;
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
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.getAnnounceFormat()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", tl("announce"));
                    }
                }

                if (type == ModifyType.BROADCAST_FORMAT) {
                    try {
                        if (channel.setBroadcastFormat(input.getFormat(VALUE))) {
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.getBroadcastFormat()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", tl("broadcast"));
                    }
                }

                if (type == ModifyType.CHAT_FORMAT) {
                    try {
                        if (channel.setChatFormat(input.getFormat(VALUE))) {
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.getChatFormat()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.getChatFormat()));
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
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(),
                                    channel.getColor() + channel.getColor().name().toLowerCase()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(),
                                channel.getColor() + channel.getColor().name().toLowerCase()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidColor", input.get(VALUE));
                    }
                }

                if (type == ModifyType.CROSS_WORLD) {
                    if (channel.setCrossWorld(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), tlState(channel.isCrossWorld())));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), tlState(channel.isCrossWorld())));
                    return true;
                }

                if (type == ModifyType.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), tlState(channel.isCustomFormat())));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), tlState(channel.isCustomFormat())));
                    return true;
                }

                if (type == ModifyType.DISTANCE) {
                    if (channel.setDistance(input.getInteger(VALUE))) {
                        sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.getDistance()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.getDistance()));
                    return true;
                }

                if (type == ModifyType.OWNER) {
                    if (channel.isPersist()) {
                        sender.sendMessage(tlErr("modifyOwnerPersist", channel.getFullName()));
                        return true;
                    }

                    final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(VALUE));

                    if (target == null || !sender.canSee(target)) {
                        throw new PlayerNotFoundException(input.get(VALUE));
                    }

                    if (channel.setOwner(target.getUniqueId())) {
                        sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), target.getName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), target.getName()));
                    return true;
                }

                if (type == ModifyType.PASSWORD) {
                    if (channel.isDefault()) {
                        sender.sendMessage(tlErr("modifyPasswordDefault", channel.getFullName()));
                        return true;
                    }

                    try {
                        if (channel.setPassword(input.get(VALUE))) {
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(),
                                    channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(),
                                channel.getPassword() != null ? channel.getPassword() : input.get(VALUE)));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidPassword", input.get(VALUE));
                    }
                }

                if (type == ModifyType.SHORT_NAME) {
                    try {
                        if (channel.setShortName(input.get(VALUE))) {
                            sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.getShortName()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.getShortName()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidName", input.get(VALUE));
                    }
                }

                if (type == ModifyType.VERBOSE) {
                    if (channel.setVerbose(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tlType(type), channel.getFullName(), channel.isVerbose()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tlType(type), channel.getFullName(), channel.isVerbose()));
                    return true;
                }

                throw new InvalidArgumentException("invalidType", type.getIdentifier());
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
                }

                if (type == ModifyType.CROSS_WORLD || type == ModifyType.CUSTOM_FORMAT || type == ModifyType.VERBOSE) {
                    final List<String> completion = new ArrayList<>();

                    if (StringUtil.startsWithIgnoreCase(Boolean.FALSE.toString(), input.get(VALUE))) {
                        completion.add(Boolean.FALSE.toString());
                    }

                    if (StringUtil.startsWithIgnoreCase(Boolean.TRUE.toString(), input.get(VALUE))) {
                        completion.add(Boolean.TRUE.toString());
                    }

                    return completion;
                }

                if (type == ModifyType.OWNER) {
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
