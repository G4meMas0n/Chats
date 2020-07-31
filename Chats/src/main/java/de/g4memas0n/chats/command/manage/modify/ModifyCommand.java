package de.g4memas0n.chats.command.manage.modify;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.channel.IChannel.Modification;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ChannelNotExistException;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.InvalidArgumentException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.command.TypeNotAvailableException;
import de.g4memas0n.chats.command.TypeNotFoundException;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.permission.Permission;
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
import static de.g4memas0n.chats.messaging.Messages.tlState;

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

        this.setPermission(Permission.MODIFY.getNode());

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

                final Modification type = Modification.getType(input.get(TYPE));

                if (type == null) {
                    throw new TypeNotFoundException(input.get(TYPE));
                }

                if (sender.canModify(channel, type)) {
                    return modifier.execute(sender, input, channel, type);
                }

                sender.sendMessage(tl("modifyDeniedType", tl(type.getKey()), channel.getFullName()));
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

                for (final Modification modification : Modification.values()) {
                    if (modifier.accept(modification) && sender.canModify(channel, modification)) {
                        if (StringUtil.startsWithIgnoreCase(modification.getIdentifier(), input.get(TYPE))) {
                            completion.add(modification.getIdentifier());
                        }
                    }
                }

                return completion;
            }

            // input.getLength() > 3
            final Modification type = Modification.getType(input.get(TYPE));

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

        private final BasicCommand parent;

        private final String identifier;
        private final int minArgs;
        private final int maxArgs;

        protected Modifier(@NotNull final BasicCommand parent,
                           @NotNull final String identifier,
                           final int minArgs,
                           final int maxArgs) {
            this.parent = parent;
            this.identifier = identifier;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        public final @NotNull Chats getInstance() {
            return this.parent.getInstance();
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
        public abstract boolean accept(@NotNull final Modification type);

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
                                        @NotNull final Modification type) throws InputException;

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
                                                          @NotNull final Modification type);
    }

    /**
     * The modify remove modifier that allows to remove settings of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    public static final class RemoveModifier extends Modifier {

        protected RemoveModifier(@NotNull final BasicCommand parent) {
            super(parent, "remove", 3, 3);
        }

        @Override
        public boolean accept(@NotNull final Modification modification) {
            return modification != Modification.COLOR && modification != Modification.CROSS_WORLD
                    && modification != Modification.CUSTOM_FORMAT && modification != Modification.VERBOSE;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final Modification modification) throws InputException {
            if (input.getLength() == this.getMinArgs()) {
                if (modification == Modification.ANNOUNCE_FORMAT) {
                    if (channel.setAnnounceFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("formatAnnounce"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("formatAnnounce"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.BROADCAST_FORMAT) {
                    if (channel.setBroadcastFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("formatBroadcast"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("formatBroadcast"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.CHAT_FORMAT) {
                    if (channel.setChatFormat(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("formatChat"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("formatChat"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.DISTANCE) {
                    if (channel.setDistance(-1)) {
                        sender.sendMessage(tl("modifyRemove", tl("distance"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("distance"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.OWNER) {
                    if (channel.setOwner(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("owner"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("owner"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.PASSWORD) {
                    if (channel.setPassword(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("password"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("password"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.SHORT_NAME) {
                    if (channel.setShortName(null)) {
                        sender.sendMessage(tl("modifyRemove", tl("shortName"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyRemoveAlready", tl("shortName"), channel.getFullName()));
                    return true;
                }

                throw new TypeNotAvailableException(modification);
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final Modification modification) {
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

        protected ResetModifier(@NotNull final BasicCommand parent) {
            super(parent, "reset", 3, 3);
        }

        @Override
        public boolean accept(@NotNull final Modification modification) {
            return modification == Modification.COLOR || modification == Modification.CROSS_WORLD
                    || modification == Modification.CUSTOM_FORMAT || modification == Modification.VERBOSE;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final Modification modification) throws InputException {
            if (input.getLength() == this.getMaxArgs()) {
                if (modification == Modification.COLOR) {
                    if (channel.setColor(null)) {
                        sender.sendMessage(tl("modifyReset", tl("color"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tl("color"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.CROSS_WORLD) {
                    if (channel.setCrossWorld(true)) {
                        sender.sendMessage(tl("modifyReset", tl("crossWorld"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tl("crossWorld"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(false)) {
                        sender.sendMessage(tl("modifyReset", tl("customFormat"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tl("customFormat"), channel.getFullName()));
                    return true;
                }

                if (modification == Modification.VERBOSE) {
                    if (channel.setVerbose(true)) {
                        sender.sendMessage(tl("modifyReset", tl("verbose"), channel.getFullName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifyResetAlready", tl("verbose"), channel.getFullName()));
                    return true;
                }

                throw new TypeNotAvailableException(modification);
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final Modification modification) {
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

        protected SetModifier(@NotNull final BasicCommand parent) {
            super(parent, "set", 4, -1);
        }

        @Override
        public boolean accept(@NotNull final Modification modification) {
            return true;
        }

        @Override
        public boolean execute(@NotNull final ICommandSource sender,
                               @NotNull final ICommandInput input,
                               @NotNull final IChannel channel,
                               @NotNull final Modification modification) throws InputException {
            if (input.getLength() >= this.getMinArgs()) {
                final String format = input.getMessage(VALUE);

                if (modification == Modification.ANNOUNCE_FORMAT) {
                    try {
                        if (channel.setAnnounceFormat(format)) {
                            sender.sendMessage(tl("modifySet", tl("formatAnnounce"), channel.getFullName(), format));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tl("formatAnnounce"), channel.getFullName(), format));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", Placeholder.MESSAGE.toString());
                    }
                }

                if (modification == Modification.BROADCAST_FORMAT) {
                    try {
                        if (channel.setBroadcastFormat(format)) {
                            sender.sendMessage(tl("modifySet", tl("formatBroadcast"), channel.getFullName(), format));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tl("formatBroadcast"), channel.getFullName(), format));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidFormat", Placeholder.MESSAGE.toString());
                    }
                }

                if (modification == Modification.CHAT_FORMAT) {
                    try {
                        if (channel.setChatFormat(format)) {
                            sender.sendMessage(tl("modifySet", tl("formatChat"), channel.getFullName(), format));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tl("formatChat"), channel.getFullName(), format));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        if (format.contains(Placeholder.SENDER.toString())) {
                            throw new InvalidArgumentException(ex, "invalidFormat", Placeholder.MESSAGE.toString());
                        } else {
                            throw new InvalidArgumentException(ex, "invalidFormat", Placeholder.SENDER.toString());
                        }
                    }
                }
            }

            if (input.getLength() == this.getMinArgs()) {
                if (modification == Modification.COLOR) {
                    try {
                        final ChatColor color = input.getChatColor(VALUE);

                        if (channel.setColor(color)) {
                            sender.sendMessage(tl("modifySet", tl("color"), channel.getFullName(),
                                    color.toString() + color.name().charAt(0) + color.name().substring(1).toLowerCase()));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tl("color"), channel.getFullName(),
                                color.toString() + color.name().charAt(0) + color.name().substring(1).toLowerCase()));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidColor");
                    }
                }

                if (modification == Modification.CROSS_WORLD) {
                    if (channel.setCrossWorld(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tl("crossWorld"), channel.getFullName(), tlState(channel.isCrossWorld())));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("crossWorld"), channel.getFullName(), tlState(channel.isCrossWorld())));
                    return true;
                }

                if (modification == Modification.CUSTOM_FORMAT) {
                    if (channel.setCustomFormat(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tl("customFormat"), channel.getFullName(), tlState(channel.isCustomFormat())));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("customFormat"), channel.getFullName(), tlState(channel.isCustomFormat())));
                    return true;
                }

                if (modification == Modification.DISTANCE) {
                    if (channel.setDistance(input.getInteger(VALUE))) {
                        sender.sendMessage(tl("modifySet", tl("distance"), channel.getFullName(), channel.getDistance()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("distance"), channel.getFullName(), channel.getDistance()));
                    return true;
                }

                if (modification == Modification.OWNER) {
                    if (channel.isPersist()) {
                        throw new InvalidArgumentException("modifyPersist");
                    }

                    final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(VALUE));

                    if (target == null || !sender.canSee(target)) {
                        throw new PlayerNotFoundException(input.get(VALUE));
                    }

                    if (channel.setOwner(target.getUniqueId())) {
                        sender.sendMessage(tl("modifySet", tl("owner"), channel.getFullName(), target.getName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("owner"), channel.getFullName(), target.getName()));
                    return true;
                }

                if (modification == Modification.PASSWORD) {
                    if (channel.isDefault()) {
                        throw new InvalidArgumentException("modifyDefault");
                    }

                    final String password = input.get(VALUE);

                    try {
                        if (channel.setPassword(input.get(VALUE))) {
                            sender.sendMessage(tl("modifySet", tl("password"), channel.getFullName(), password));
                            return true;
                        }

                        sender.sendMessage(tl("modifySetAlready", tl("password"), channel.getFullName(), password));
                        return true;
                    } catch (IllegalArgumentException ex) {
                        throw new InvalidArgumentException(ex, "invalidPassword");
                    }
                }

                if (modification == Modification.SHORT_NAME) {
                    if (channel.setShortName(input.get(VALUE))) {
                        sender.sendMessage(tl("modifySet", tl("shortName"), channel.getFullName(), channel.getShortName()));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("shortName"), channel.getFullName(), channel.getShortName()));
                    return true;
                }

                if (modification == Modification.VERBOSE) {
                    if (channel.setVerbose(input.getBoolean(VALUE))) {
                        sender.sendMessage(tl("modifySet", tl("verbose"), channel.getFullName(), tlState(channel.isVerbose())));
                        return true;
                    }

                    sender.sendMessage(tl("modifySetAlready", tl("verbose"), channel.getFullName(), tlState(channel.isVerbose())));
                    return true;
                }

                throw new InvalidArgumentException("typeNotAvailable", modification);
            }

            return false;
        }

        @Override
        public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                 @NotNull final ICommandInput input,
                                                 @NotNull final IChannel channel,
                                                 @NotNull final Modification modification) {
            if (input.getLength() == VALUE + 1) {
                if (modification == Modification.COLOR) {
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

                if (modification == Modification.CROSS_WORLD
                        || modification == Modification.CUSTOM_FORMAT
                        || modification == Modification.VERBOSE) {
                    final List<String> completion = new ArrayList<>();

                    if (StringUtil.startsWithIgnoreCase("disable", input.get(VALUE))) {
                        completion.add("disable");
                    }

                    if (StringUtil.startsWithIgnoreCase("enable", input.get(VALUE))) {
                        completion.add("enable");
                    }

                    if (StringUtil.startsWithIgnoreCase("false", input.get(VALUE))) {
                        completion.add("false");
                    }

                    if (StringUtil.startsWithIgnoreCase("true", input.get(VALUE))) {
                        completion.add("true");
                    }

                    return completion;
                }

                if (modification == Modification.OWNER) {
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
