package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.event.ChatterChatChannelEvent;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;

/**
 * Representation of a global channel, implements the {@link IChannel} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: October 1st, 2019
 */
public final class GlobalChannel implements IChannel {

    /**
     * the channel prefix. Used to detect to which channel a log belongs.
     */
    private static final String CHANNEL_PREFIX = "[%s]";

    /**
     * the default announce format. Used for all channel announces.
     */
    private static String defaultAnnounceFormat = "{color}{message}";

    /**
     * the default broadcast format. Used for all channel broadcasts.
     */
    private static String defaultBroadcastFormat = "{color}[{bc-prefix}{color}] {message}";

    /**
     * the default channel format. Used for all channels chats.
     */
    private static String defaultChannelFormat = "{color}[{nick}]{sender}{color}: {message}";

    /**
     * the default conversion chat color. Used for all channels without a defined chat color.
     */
    private static ChatColor defaultChannelColor = ChatColor.WHITE;

    private final IChannelStorage storage;
    private final IChannelFormatter formatter;
    private final String fullName;

    private String shortName;
    private String password;
    private String customAnnounceFormat;
    private String customBroadcastFormat;
    private String customChannelFormat;
    private boolean crossWorld;
    private boolean useCustomFormat;
    private int distance;
    private ChatColor chatColor;
    private Set<IChatter> chatters;

    public GlobalChannel(@NotNull final IChannelStorage storage,
                         @NotNull final String fullName) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Empty channel full Name");
        }

        this.storage = storage;
        this.formatter = new ChannelFormatter(this);
        this.fullName = fullName;

        this.chatters = new HashSet<>();

        this.shortName = null;
        this.chatColor = ChatColor.WHITE;
        this.crossWorld = true;
        this.distance = -1;
        this.password = null;
        this.customAnnounceFormat = null;
        this.customBroadcastFormat = null;
        this.customChannelFormat = null;
        this.useCustomFormat = false;
    }

    // Methods for Channel Properties:
    @Override
    public @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public @NotNull String getShortName() {
        return this.shortName != null && !this.shortName.isEmpty() ? this.shortName : this.fullName;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        if (this.shortName.equals(shortName)) {
            return false;
        }

        this.shortName = shortName;
        this.update();
        return true;
    }

    protected void _setShortName(@NotNull final String shortName) {
        if (this.shortName.equals(shortName)) {
            return;
        }

        this.shortName = shortName;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        return this.chatColor;
    }

    @Override
    public boolean setChatColor(@NotNull final ChatColor color) {
        if (this.chatColor == color) {
            return false;
        }

        this.chatColor = color;
        this.update();
        return true;
    }

    protected void _setChatColor(@NotNull final ChatColor color) {
        if (this.chatColor == color) {
            return;
        }

        this.chatColor = color;
    }

    @Override
    public @NotNull String toString() {
        return this.fullName;
    }

    @Override
    public boolean equals(@NotNull final Object object) {
        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final GlobalChannel channel = (GlobalChannel) object;
        return this.getFullName().equals(channel.getFullName());
    }

    @Override
    public int hashCode() {
        final int prime = 59;
        int result = 5;

        result = prime * result + this.fullName.hashCode();

        return result;
    }

    // Methods for Channel Types:
    @Override
    public boolean isPersistChannel() {
        return this.storage != null;
    }

    @Override
    public boolean isGlobalChannel() {
        return true;
    }

    @Override
    public boolean isConversionChannel() {
        return false;
    }

    // Methods for Channel Settings:
    @Override
    public boolean isCrossWorld() {
        return this.crossWorld;
    }

    @Override
    public boolean setCrossWorld(final boolean enabled) {
        if (this.crossWorld == enabled) {
            return false;
        }

        this.crossWorld = enabled;
        this.update();
        return true;
    }

    protected void _setCrossWorld(final boolean enabled) {
        if (this.crossWorld == enabled) {
            return;
        }

        this.crossWorld = enabled;
    }

    @Override
    public boolean hasDistance() {
        return this.distance > 0;
    }

    @Override
    public int getDistance() {
        return this.hasDistance() ? this.distance : -1;
    }

    @Override
    public boolean setDistance(final int distance) {
        if (this.distance == distance) {
            return false;
        }

        this.distance = distance;
        this.update();
        return true;
    }

    protected void _setDistance(final int distance) {
        if (this.distance == distance) {
            return;
        }

        this.distance = distance;
    }

    @Override
    public boolean hasPassword() {
        return this.password != null && !this.password.isEmpty();
    }

    @Override
    public @Nullable String getPassword() {
        return this.password;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        if (this.password.equals(password)) {
            return false;
        }

        this.password = password;
        this.update();
        return true;
    }

    protected void _setPassword(@NotNull final String password) {
        if (this.password.equals(password)) {
            return;
        }

        this.password = password;
    }

    // Methods for Channel Formatting
    @Override
    public @NotNull IChannelFormatter getFormatter() {
        return this.formatter;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        if (this.useCustomFormat) {
            return this.customAnnounceFormat != null && !this.customAnnounceFormat.isEmpty()
                    ? this.customAnnounceFormat : defaultAnnounceFormat;
        }

        return defaultAnnounceFormat;
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        if (this.customAnnounceFormat.equals(format)) {
            return false;
        }

        this.customAnnounceFormat = format;
        this.update();
        return true;
    }

    protected void _setAnnounceFormat(@NotNull final String format) {
        if (this.customAnnounceFormat.equals(format)) {
            return;
        }

        this.customAnnounceFormat = format;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        if (this.useCustomFormat) {
            return this.customBroadcastFormat != null && !this.customBroadcastFormat.isEmpty()
                    ? this.customBroadcastFormat : defaultBroadcastFormat;
        }

        return defaultBroadcastFormat;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        if (this.customBroadcastFormat.equals(format)) {
            return false;
        }

        this.customBroadcastFormat = format;
        this.update();
        return true;
    }

    protected void _setBroadcastFormat(@NotNull final String format) {
        if (this.customBroadcastFormat.equals(format)) {
            return;
        }

        this.customBroadcastFormat = format;
    }

    @Override
    public @NotNull String getChannelFormat() {
        if (this.useCustomFormat) {
            return this.customChannelFormat != null && !this.customChannelFormat.isEmpty()
                    ? this.customChannelFormat : defaultChannelFormat;
        }

        return defaultChannelFormat;
    }

    @Override
    public boolean setChannelFormat(@Nullable final String format) {
        if (this.customChannelFormat.equals(format)) {
            return false;
        }

        this.customChannelFormat = format;
        this.update();
        return true;
    }

    protected void _setChannelFormat(@NotNull final String format) {
        if (this.customChannelFormat.equals(format)) {
            return;
        }

        this.customChannelFormat = format;
    }

    public boolean isUseCustomFormat() {
        return this.useCustomFormat;
    }

    public boolean setUseCustomFormat(final boolean enabled) {
        if (this.useCustomFormat == enabled) {
            return false;
        }

        this.useCustomFormat = enabled;
        this.update();
        return true;
    }

    protected void _setUseCustomFormat(final boolean enabled) {
        if (this.useCustomFormat == enabled) {
            return;
        }

        this.useCustomFormat = enabled;
    }

    // Methods for Chatter Collection of this Channel:
    @Override
    public @NotNull Set<IChatter> getChatters() {
        return this.chatters;
    }

    @Override
    public boolean addChatter(@NotNull final IChatter chatter) {
        if (this.chatters.contains(chatter)) {
            return false;
        }

        this.chatters.add(chatter);
        return true;
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.contains(chatter)) {
            return false;
        }

        this.chatters.remove(chatter);
        return true;
    }

    // Methods for performing Actions:
    @Override
    public void performAnnounce(@NotNull final String message) {
        IChats instance = Chats.getInstance();

        final String output = this.formatter.formatAnnounce(message);

        if (instance != null) {
            String log = output;

            if (!this.formatter.isAnnounceLoggable()) {
                log = String.format(CHANNEL_PREFIX, this.fullName) + log;
            }

            instance.logChat(log);
        }

        for (IChatter current : this.chatters) {
            current.getPlayer().sendMessage(output);
        }
    }

    @Override
    public void performBroadcast(@NotNull final String message) {
        IChats instance = Chats.getInstance();

        final String output = this.formatter.formatBroadcast(message);

        if (instance != null) {
            String log = output;

            if (!this.formatter.isBroadcastLoggable()) {
                log = String.format(CHANNEL_PREFIX, this.fullName) + log;
            }

            instance.logChat(log);
        }

        for (IChatter current : this.chatters) {
            current.getPlayer().sendMessage(output);
        }
    }

    @Override
    public void performChat(@NotNull final IChatter sender,
                            @NotNull final String message) throws IllegalArgumentException {
        if (!this.chatters.contains(sender)) {
            throw new IllegalArgumentException("The sender must be in the channel: " + this.getFullName());
        }

        IChats instance = Chats.getInstance();

        ChatterChatChannelEvent event = new ChatterChatChannelEvent(sender, this, message);

        if (instance != null) {
            instance.getPluginManager().callEvent(event);
        }

        if (event.isCancelled()) {
            return;
        }

        final String output = this.formatter.formatChat(sender, event.getMessage());

        if (instance != null) {
            String log = output;

            if (!this.formatter.isChatLoggable()) {
                log = String.format(CHANNEL_PREFIX, this.fullName) + log;
            }

            instance.logChat(log);
        }

        final Location locationSender = sender.getPlayer().getLocation();
        final World worldSender = sender.getPlayer().getWorld();

        if (this.hasDistance()) {
            for (IChatter current : this.chatters) {
                final Player crtPlayer = current.getPlayer();

                if (!crtPlayer.getWorld().equals(worldSender)) {
                    continue;
                }

                if (crtPlayer.getLocation().distance(locationSender) <= this.distance) {
                    crtPlayer.sendMessage(output);
                }
            }

            return;
        }

        if (!this.isCrossWorld()) {
            for (IChatter current : this.chatters) {
                final Player crtPlayer = current.getPlayer();

                if (crtPlayer.getWorld().equals(worldSender)) {
                    crtPlayer.sendMessage(output);
                }
            }

            return;
        }

        for (IChatter current : this.chatters) {
            current.getPlayer().sendMessage(output);
        }
    }

    // Method for Storage update:
    private void update() {
        if (this.isPersistChannel()) {
            this.storage.update(this);
        }
        this.formatter.update();
    }

    // Static Methods:
    public static @NotNull String getDefaultAnnounceFormat() {
        return GlobalChannel.defaultAnnounceFormat;
    }

    public static boolean setDefaultAnnounceFormat(@NotNull final String format) {
        if (GlobalChannel.defaultAnnounceFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultAnnounceFormat = format;
        return true;
    }

    public static @NotNull String getDefaultBroadcastFormat() {
        return GlobalChannel.defaultBroadcastFormat;
    }

    public static boolean setDefaultBroadcastFormat(@NotNull final String format) {
        if (GlobalChannel.defaultBroadcastFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultBroadcastFormat = format;
        return true;
    }

    public static @NotNull String getDefaultChannelFormat() {
        return GlobalChannel.defaultChannelFormat;
    }

    public static boolean setDefaultChannelFormat(@NotNull final String format) {
        if (GlobalChannel.defaultChannelFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultChannelFormat = format;
        return true;
    }

    public static @NotNull ChatColor getDefaultChannelColor() {
        return GlobalChannel.defaultChannelColor;
    }

    public static boolean setDefaultChannelColor(@NotNull final ChatColor color) {
        if (GlobalChannel.defaultChannelColor == color) {
            return false;
        }

        GlobalChannel.defaultChannelColor = color;
        return true;
    }
}
