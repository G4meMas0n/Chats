package de.g4memas0n.chats.channel;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Placeholder;
import de.g4memas0n.chats.util.IType;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Set;
import java.util.UUID;

/**
 * Channel Interface that defines a channel representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface IChannel extends Comparable<IChannel> {

    // Channel Properties Methods:
    /**
     * Returns the full name of this channel.
     *
     * @return the full name.
     */
    @NotNull String getFullName();

    /**
     * Returns the full name colored with the chat color of this channel.
     *
     * @return the colored full name.
     */
    @NotNull String getColoredName();

    /**
     * Returns the short name of this channel.
     *
     * <p>When this channel has no short name, the full name of this channel will be returned.</p>
     *
     * @return the short name.
     */
    @NotNull String getShortName();

    /**
     * Sets a new short name for this channel.
     * Removes the short name when the given name is null or empty.
     *
     * @param name the new short name for this channel.
     * @return true when the short name was changed as result of this call, false otherwise.
     */
    boolean setShortName(@Nullable final String name);

    /**
     * Returns the chat color of this channel.
     * @return the chat color.
     */
    @NotNull ChatColor getColor();

    /**
     * Sets a new chat color for this channel.
     * Resets the chat color when the given color is null.
     *
     * @param color the new chat color for this channel.
     * @return true when the chat color was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given chat color is not a color.
     */
    boolean setColor(@Nullable final ChatColor color) throws IllegalArgumentException;

    /**
     * Returns whether this channel has a password or not.
     *
     * @return true when a valid password is set, false otherwise.
     */
    boolean hasPassword();

    /**
     * Returns the password of this channel.
     *
     * @return the password or null when this channel has no password.
     */
    @Nullable String getPassword();

    /**
     * Sets a new password for this channel. Removes it when the given password is null or empty.
     *
     * <p>The given password must be at least three characters long, otherwise it will throw an exception.</p>
     *
     * @param password the new password for this channel.
     * @return true when the password was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given password is shorter as the minimum character count.
     */
    boolean setPassword(@Nullable final String password) throws IllegalArgumentException;

    /**
     * Returns whether this channel has a distance.
     *
     * @return true when the distance of this channel is bigger than zero, false otherwise.
     */
    boolean hasDistance();

    /**
     * Returns the distance of this channel.
     *
     * @return the distance, or -1 when this channel has no distance.
     */
    int getDistance();

    /**
     * Sets a new distance for this channel.
     * Removes it when the given distance is smaller than one.
     *
     * @param distance the new distance for this channel.
     * @return true when the distance was changed as result of this call, false otherwise.
     */
    boolean setDistance(final int distance);

    /**
     * Returns whether this channel is cross world or not.
     *
     * @return true when this channel is world across and do not have a distance, false otherwise.
     */
    boolean isCrossWorld();

    /**
     * Sets whether this channel is cross world or not.
     *
     * @param crossWorld true if this channel is world across.
     * @return true when the option was changed as result of this call, false otherwise.
     */
    boolean setCrossWorld(final boolean crossWorld);

    /**
     * Returns whether this channel will announce channel joins and leaves.
     *
     * @return true when this channel will announce joins joins and leaves.
     */
    boolean isVerbose();

    /**
     * Sets whether this channel will announce channel joins and leaves.
     *
     * @param verbose true if this channel should announce channel joins and leaves.
     * @return true when the option was changed as result of this call, false otherwise.
     */
    boolean setVerbose(final boolean verbose);

    /**
     * Returns whether this channel uses custom formats or not.
     *
     * @return true when it uses custom formats, false otherwise.
     */
    boolean isCustomFormat();

    /**
     * Sets whether this channel uses custom formats or the default formats.
     *
     * @param customFormat true when this channel uses custom formats.
     * @return true when the option was changed as result of this call, false otherwise.
     */
    boolean setCustomFormat(final boolean customFormat);

    /**
     * Returns the announce format of this channel.
     *
     * @return the custom announce format if it exists, otherwise the default announce format.
     */
    @NotNull String getAnnounceFormat();

    /**
     * Sets the custom announce format for this channel.
     * Removes it when the given format is null or empty.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an
     * exception.</p>
     *
     * @param format the new announce format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setAnnounceFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the broadcast format of this channel.
     *
     * @return the custom broadcast format it it exists, otherwise the default broadcast format.
     */
    @NotNull String getBroadcastFormat();

    /**
     * Sets the custom broadcast format for this channel.
     * Removes it when the given format is null or empty.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} placeholder, otherwise it will throw an
     * exception.</p>
     *
     * @param format the new broadcast format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setBroadcastFormat(@Nullable final String format) throws IllegalArgumentException;

    /**
     * Returns the chat format that of this channel.
     *
     * @return the custom chat format if it exists, otherwise the default chat format.
     */
    @NotNull String getChatFormat();

    /**
     * Sets the custom chat format for this channel.
     * Removes it when the given format is null or empty.
     *
     * <p>The given format must include the {@link Placeholder#MESSAGE} and {@link Placeholder#SENDER} placeholder,
     * otherwise it will throw an exception.</p>
     *
     * @param format the new channel format for this channel.
     * @return true when the format was changed as result of this call, false otherwise.
     * @throws IllegalArgumentException Thrown when the given format does not include the required placeholders.
     */
    boolean setChatFormat(@Nullable final String format) throws IllegalArgumentException;


    // Channel Type Methods:
    /**
     * Returns the type of this channel.
     *
     * @return the type.
     */
    @NotNull Type getType();

    /**
     * Returns whether this channel represents a conversion channel or not.
     *
     * @return true when it represents a conversation channel, false otherwise.
     */
    boolean isConversation();

    /**
     * Returns whether this channel represents a persist channel or not.
     *
     * @return true when it represents a persist channel, false otherwise.
     */
    boolean isPersist();

    /**
     * Returns whether this channel represents a standard channel or not.
     *
     * @return true when it represents a standard channel, false otherwise.
     */
    boolean isStandard();

    /**
     * Returns whether this channel is the default channel or not.
     *
     * @return true when it is the default channel, false otherwise.
     */
    boolean isDefault();

    // Channel Collection Methods:
    /**
     * Returns all members of this channel.
     *
     * @return the members.
     */
    @NotNull Set<IChatter> getMembers();

    /**
     * Sets a member for this channel.
     *
     * <p><b>Please do not use this method unless you know exactly what you are doing.</b></p>
     *
     * <p>This method will only add/remove the given chatter to/from this channel, without adding/removing this channel
     * to/from the given chatter and without announcing the join/leave.</p>
     *
     * <p>For correctly adding/removing chatters to/from a channel, use following channel methods:<br>
     *  - {@link IChannel#addMember(IChatter) addMember(IChatter chatter)} or
     * {@link IChannel#addMember(IChatter, boolean) addMember(IChatter chatter, boolean silent)}<br>
     *  - {@link IChannel#removeMember(IChatter) removeMember(IChatter chatter)} or
     * {@link IChannel#removeMember(IChatter, boolean) removeMember(IChatter chatter, boolean silent)}</p>
     *
     * <p>Otherwise the following chatter methods can also be used:<br>
     *  - {@link IChatter#joinChannel(IChannel) joinChannel(IChannel channel)} or
     * {@link IChatter#joinChannel(IChannel, boolean) joinChannel(IChannel channel, boolean silent)}<br>
     *  - {@link IChatter#leaveChannel(IChannel) leaveChannel(IChannel channel)} or
     * {@link IChatter#leaveChannel(IChannel, boolean) leaveChannel(IChannel channel, boolean silent)}</p>
     *
     * @param chatter the chatter to set.
     * @param member true to add the chatter, false to remove the chatter.
     * @return true when the chatter was added or remove as result of this call, false otherwise.
     */
    boolean setMember(@NotNull final IChatter chatter, final boolean member);

    /**
     * Adds a new member to this channel.
     *
     * <p>This method will add this channel to the given chatter when it is not already added.</p>
     *
     * @param chatter the member to add.
     * @return true when the member was added as result of this call, false otherwise.
     * @see IChatter#joinChannel(IChannel)
     */
    boolean addMember(@NotNull final IChatter chatter);

    /**
     * Adds a new member to this channel.
     *
     * <p>This method will add this channel to the given chatter when it is not already added.</p>
     *
     * @param chatter the member to add.
     * @param silent true when the member should added silently.
     * @return true when the member was added as result of this call, false otherwise.
     * @see IChatter#joinChannel(IChannel, boolean)
     */
    boolean addMember(@NotNull final IChatter chatter, final boolean silent);

    /**
     * Removes a member from this channel.
     *
     * <p>This method will remove this channel from the given chatter when it is not already removed.</p>
     *
     * @param chatter the member to remove.
     * @return true when the member was removed as result of this call, false otherwise.
     * @see IChatter#leaveChannel(IChannel)
     */
    boolean removeMember(@NotNull final IChatter chatter);

    /**
     * Removes a member from this channel.
     *
     * <p>This method will remove this channel from the given chatter when it is not already removed.</p>
     *
     * @param chatter the member to remove.
     * @param silent true when the member should removed silently.
     * @return true when the member was removed as result of this call, false otherwise.
     * @see IChatter#leaveChannel(IChannel, boolean)
     */
    boolean removeMember(@NotNull final IChatter chatter, final boolean silent);

    /**
     * Returns whether the given chatter is a member of this channel or not.
     *
     * @param chatter the chatter to check.
     * @return true when the given chatter is a member, false otherwise.
     */
    boolean isMember(@NotNull final IChatter chatter);

    /**
     * Bans a member from this channel.
     *
     * <p>This method will remove this channel from the given chatter and bans the uniqueId of the given chatter from
     * this channel.</p>
     *
     * @param member the member to ban.
     * @return true when the member was banned as result of this call, false otherwise.
     */
    boolean banMember(@NotNull final IChatter member);

    /**
     * Kicks a member from this channel.
     *
     * <p>This method will remove this channel from the given chatter when it is not already removed.</p>
     *
     * @param member the member to kick.
     * @return true when the member was kicked as result of this call, false otherwise.
     */
    boolean kickMember(@NotNull final IChatter member);

    /**
     * Mutes a member in this channel.
     *
     * <p>This method will mutes the given chatter in this channel, when it is not already muted.</p>
     *
     * @param member the member to mute.
     * @return true when the member was muted as result of this call, false otherwise.
     */
    boolean muteMember(@NotNull final IChatter member);

    /**
     * Pardons a banned chatter in this channel.
     *
     * <p>This method will pardon the banned uniqueId of the given offline chatter.</p>
     *
     * @param chatter the banned chatter to pardon.
     * @return true when the banned chatter was pardoned as result of this call, false otherwise.
     */
    boolean pardonMember(@NotNull final IOfflineChatter chatter);

    /**
     * Unmutes a muted member in this channel.
     *
     * <p>This method will unmute the given channel in this channel, when it is not already unmuted.</p>
     *
     * @param member the muted member to unmute.
     * @return true when the member was unmuted as result of this call, false otherwise.
     */
    boolean unmuteMember(@NotNull final IOfflineChatter member);

    /**
     * Returns all banned members of this channel.
     *
     * @return the uniqueIds of the banned members.
     */
    @NotNull Set<UUID> getBans();

    /**
     * Sets a banned member for this channel.
     *
     * @param uniqueId the uniqueId of the member to set.
     * @param banned true to ban the member, false to pardon.
     * @return true when the member was banned or pardoned as result of this call, false otherwise.
     */
    boolean setBanned(@NotNull final UUID uniqueId, final boolean banned);

    /**
     * Returns whether the given uniqueId is banned from this channel or not.
     *
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is banned, false otherwise.
     */
    boolean isBanned(@NotNull final UUID uniqueId);

    /**
     * Returns all muted members of this channel.
     *
     * @return the uniqueIds of the muted members.
     */
    @NotNull Set<UUID> getMutes();

    /**
     * Sets a muted chatters for this channel.
     *
     * @param uniqueId the uniqueId of the member to set.
     * @param muted true to mute the chatter, false to unmute.
     * @return true when the member was muted or unmuted as result of this call, false otherwise.
     */
    boolean setMuted(@NotNull final UUID uniqueId, final boolean muted);

    /**
     * Returns whether the given uniqueId is muted in this channel or not.
     *
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is muted, false otherwise.
     */
    boolean isMuted(@NotNull final UUID uniqueId);

    /**
     * Returns whether this channel has a owner or not.
     *
     * @return true when a owner is set, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean hasOwner();

    /**
     * Returns the owner of this channel.
     *
     * @return the uniqueId of the owner, or null when this channel has no owner.
     */
    @Nullable UUID getOwner();

    /**
     * Sets the owner of this channel. Removes the owner when the given uniqueId is null.
     *
     * @param uniqueId the uniqueId of the owner to set.
     * @return true when the owner was changed as result of this call, false otherwise.
     */
    boolean setOwner(@Nullable final UUID uniqueId);

    /**
     * Returns whether the given uniqueId is the owner of this channel.
     *
     * @param uniqueId the uniqueId of the chatter to check.
     * @return true when the given uniqueId is the owner, false otherwise.
     */
    boolean isOwner(@NotNull final UUID uniqueId);

    // Performing Methods:
    /**
     * Performs the announce action.
     * Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given announce message.
     *
     * <p><i><b>Note:</b> This method can be called asynchronous.</i></p>
     *
     * @param message the announce message.
     */
    void performAnnounce(@NotNull final String message);

    /**
     * Performs the broadcast action.
     *
     * <p>Checks all conditions to perform this action successfully and then sends all chatters of this channel the
     * given broadcast message.</p>
     *
     * <p><i><b>Note:</b> This method can be called asynchronous.</i></p>
     *
     * @param message the broadcast message.
     */
    void performBroadcast(@NotNull final String message);

    /**
     * Performs the chat action.
     *
     * <p>Checks all conditions to perform this action successfully and then sends all chatters of this channel that
     * can see the given chat message from the given sender.</p>
     *
     * <p><i><b>Note:</b> This method can be called asynchronous.</i></p>
     *
     * @param sender the sender of the given message.
     * @param message the chat message from the sender.
     */
    void performChat(@NotNull final IChatter sender, @NotNull final String message);

    /**
     * Information Enum for all information types of a channel.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    enum Information implements IType {

        /*
         * Represents the bans info of a channel
         */
        //BANS("bans"),

        /**
         * Represents the color info of a channel.
         */
        COLOR("color"),

        /**
         * Represents the cross-world info of a channel.
         */
        CROSS_WORLD("cross-world"),

        /**
         * Represents the distance info of a channel.
         */
        DISTANCE("distance"),

        /**
         * Represents the formats info of a channel.
         */
        FORMATS("formats"),

        /**
         * Represents the mutes info of a channel.
         */
        MUTES("mutes"),

        /**
         * Represents the owner info of a channel.
         */
        OWNER("owner"),

        /**
         * Represents the password info of a channel.
         */
        PASSWORD("password"),

        /**
         * Represents the short-name info of a channel.
         */
        SHORT_NAME("short-name"),

        /**
         * Represents the channel type info of a channel.
         */
        TYPE("type"),

        /**
         * Represents the verbose info of a channel.
         */
        VERBOSE("verbose");

        private final String identifier;

        Information(@NotNull final String identifier) {
            this.identifier = identifier;
        }

        @Override
        public @NotNull String getIdentifier() {
            return this.identifier;
        }

        @Override
        public @NotNull String getKey() {
            final int index = this.identifier.indexOf("-");

            if (index < 0) {
                return this.identifier;
            }

            if (index + 1 == this.identifier.length()) {
                return this.identifier.substring(0, index);
            }

            return this.identifier.substring(0, index)
                    + this.identifier.substring(index + 1, index + 2).toUpperCase()
                    + this.identifier.substring(index + 2);
        }

        @Override
        public final @NotNull String toString() {
            return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.getKey() + "}";
        }
    }

    /**
     * Modification Enum for all options of a channel that can be modified.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    enum Modification implements IType {

        /**
         * Represents the announce format modify option of a channel.
         */
        ANNOUNCE_FORMAT("announce-format", "formatAnnounce"),

        /**
         * Represents the broadcast-format modify option of a channel.
         */
        BROADCAST_FORMAT("broadcast-format", "formatBroadcast"),

        /**
         * Represents the chat-format modify option of a channel.
         */
        CHAT_FORMAT("chat-format", "formatChat"),

        /**
         * Represents the color modify option of a channel.
         */
        COLOR("color"),

        /**
         * Represents the cross-world modify option of a channel.
         */
        CROSS_WORLD("cross-world"),

        /**
         * Represents the custom-format modify option of a channel.
         */
        CUSTOM_FORMAT("custom-format"),

        /**
         * Represents the distance modify option of a channel.
         */
        DISTANCE("distance"),

        /**
         * Represents the owner modify option of a channel.
         */
        OWNER("owner"),

        /**
         * Represents the password modify option of a channel.
         */
        PASSWORD("password"),

        /**
         * Represents the short-name modify option of a channel.
         */
        SHORT_NAME("short-name"),

        /**
         * Represents the verbose modify option of a channel.
         */
        VERBOSE("verbose");

        private final String identifier;
        private final String key;

        Modification(@NotNull final String identifier) {
            this.identifier = identifier;
            this.key = null;
        }

        Modification(@NotNull final String identifier,
                     @NotNull final String key) {
            this.identifier = identifier;
            this.key = key;
        }

        @Override
        public @NotNull String getIdentifier() {
            return this.identifier;
        }

        @Override
        public @NotNull String getKey() {
            if (this.key == null) {
                final int index = this.identifier.indexOf("-");

                if (index < 0) {
                    return this.identifier;
                }

                if (index + 1 == this.identifier.length()) {
                    return this.identifier.substring(0, index);
                }

                return this.identifier.substring(0, index)
                        + this.identifier.substring(index + 1, index + 2).toUpperCase()
                        + this.identifier.substring(index + 2);
            }

            return this.key;
        }

        @Override
        public final @NotNull String toString() {
            return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.getKey() + "}";
        }

        /**
         * Returns the modification type with the given identifier.
         *
         * <p>Can be null when there is no modification type with the given identifier.</p>
         *
         * @param identifier the identifier to search for the type.
         * @return the modification type with the given identifier or null if there is no with the given identifier.
         */
        public static @Nullable Modification getType(@NotNull final String identifier) {
            for (final Modification modification : Modification.values()) {
                if (modification.getIdentifier().equalsIgnoreCase(identifier)) {
                    return modification;
                }
            }

            return null;
        }
    }

    /**
     * Type Enum for all types of channels.
     *
     * @author G4meMas0n
     * @since Release 1.0.0
     */
    enum Type implements IType {

        /**
         * Represents a conversation channel.
         */
        CONVERSATION("conversation"),

        /**
         * Represents a persistent channel.
         */
        PERSIST("persist"),

        /**
         * Represents a standard not persistent channel.
         */
        STANDARD("standard");

        private final String identifier;

        Type(@NotNull final String identifier) {
            this.identifier = identifier;
        }

        @Override
        public @NotNull String getIdentifier() {
            return this.identifier;
        }

        @Override
        public @NotNull String getKey() {
            return this.identifier;
        }

        @Override
        public @NotNull String toString() {
            return this.getClass().getSimpleName() + "{identifier=" + this.identifier + ";key=" + this.identifier + "}";
        }

        /**
         * Returns the channel type with the given identifier.
         *
         * <p>Can be null when there is no channel type with the given identifier.</p>
         *
         * @param identifier the identifier to search for the type.
         * @return the channel type with the given identifier or null if there is no with the given identifier.
         */
        public static @Nullable Type getType(@NotNull final String identifier) {
            for (final Type type : Type.values()) {
                if (type.getIdentifier().equalsIgnoreCase(identifier)) {
                    return type;
                }
            }

            return null;
        }
    }
}
