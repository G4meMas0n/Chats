package de.g4memas0n.Chats.channels;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.events.ChatterChatChannelEvent;
import de.g4memas0n.Chats.formatters.AnnounceFormatter;
import de.g4memas0n.Chats.formatters.BroadcastFormatter;
import de.g4memas0n.Chats.formatters.ChannelFormatter;
import de.g4memas0n.Chats.formatters.IFormatter;
import de.g4memas0n.Chats.storages.IChannelStorage;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public final class GlobalChannel implements IChannel {
    private static String defaultAnnounceFormat = IChannel.DEFAULT_ANNOUNCE_FORMAT;
    private static String defaultBroadcastFormat = IChannel.DEFAULT_BROADCAST_FORMAT;
    private static String defaultChannelFormat = IChannel.DEFAULT_CHANNEL_FORMAT;
    private static ChatColor defaultChannelColor = ChatColor.WHITE;
    private final IChannelStorage storage;
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
                         @NotNull final String fullName,
                         @NotNull final String shortName) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Empty channel full Name");
        }

        this.fullName = fullName;
        this.shortName = shortName.isEmpty() ? fullName : shortName;
        this.storage = storage;
        this.password = null;
        this.customAnnounceFormat = IChannel.DEFAULT_ANNOUNCE_FORMAT;
        this.customBroadcastFormat = IChannel.DEFAULT_BROADCAST_FORMAT;
        this.customChannelFormat = IChannel.DEFAULT_CHANNEL_FORMAT;
        this.useCustomFormat = false;
        this.crossWorld = true;
        this.chatColor = ChatColor.WHITE;
        this.chatters = new HashSet<>();
    }

    public GlobalChannel(@NotNull final IChannelStorage storage,
                         @NotNull final String fullName,
                         @NotNull final String shortName,
                         @NotNull final String password) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Empty channel full Name");
        }

        this.fullName = fullName;
        this.shortName = shortName.isEmpty() ? fullName : shortName;
        this.storage = storage;
        this.password = password;
        this.customAnnounceFormat = IChannel.DEFAULT_ANNOUNCE_FORMAT;
        this.customBroadcastFormat = IChannel.DEFAULT_BROADCAST_FORMAT;
        this.customChannelFormat = IChannel.DEFAULT_CHANNEL_FORMAT;
        this.useCustomFormat = false;
        this.crossWorld = true;
        this.chatColor = ChatColor.WHITE;
        this.chatters = new HashSet<>();
    }

    public GlobalChannel(@NotNull final IChannelStorage storage,
                         @NotNull final String fullName,
                         @NotNull final String shortName,
                         @NotNull final String password,
                         @NotNull final String channelFormat,
                         @NotNull final String announceFormat,
                         @NotNull final String broadcastFormat,
                         final boolean useCustomFormat,
                         final boolean crossWorld,
                         final int distance,
                         @NotNull final ChatColor chatColor) throws IllegalArgumentException {
        if (fullName.isEmpty()) {
            throw new IllegalArgumentException("Empty channel full Name");
        }

        this.fullName = fullName;
        this.shortName = shortName.isEmpty() ? fullName : shortName;
        this.storage = storage;
        this.password = password.isEmpty() ? null : password;
        this.customAnnounceFormat = announceFormat;
        this.customBroadcastFormat = broadcastFormat;
        this.customChannelFormat = channelFormat;
        this.useCustomFormat = useCustomFormat;
        this.crossWorld = crossWorld;
        this.distance = distance > 0 ? distance : -1;
        this.chatColor = chatColor;
        this.chatters = new HashSet<>();
    }

    // Methods for Channel Properties:
    @Override
    @NotNull
    public String getFullName() {
        return this.fullName;
    }

    @Override
    @NotNull
    public String getShortName() {
        return this.shortName;
    }

    @Override
    public boolean setShortName(@NotNull final String shortName) {
        if (this.shortName.equals(shortName)) {
            return false;
        }

        this.shortName = shortName;
        this.update();
        return true;
    }

    @Override
    @NotNull
    public ChatColor getChatColor() {
        return this.chatColor;
    }

    @Override
    public boolean setChatColor(@NotNull final ChatColor chatColor) {
        if (this.chatColor == chatColor) {
            return false;
        }

        this.chatColor = chatColor;
        this.update();
        return true;
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
    public boolean setCrossWorld(final boolean state) {
        if (this.crossWorld == state) {
            return false;
        }

        this.crossWorld = state;
        this.update();
        return true;
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

    @Override
    public boolean hasPassword() {
        return this.password != null;
    }

    @Override
    @Nullable
    public String getPassword() {
        return this.password;
    }

    @Override
    public boolean setPassword(@NotNull final String password) {
        if (this.password.equals(password)) {
            return false;
        }

        this.password = password;
        this.update();
        return true;
    }

    @Override
    public boolean removePassword() {
        if (this.password == null) {
            return false;
        }

        this.password = null;
        this.update();
        return true;
    }

    // Methods for Custom Channel Formats:
    @Override
    public boolean isUseCustomFormat() {
        return this.useCustomFormat;
    }

    @Override
    public boolean setUseCustomFormat(final boolean state) {
        if (this.useCustomFormat == state) {
            return false;
        }

        this.useCustomFormat = state;
        this.update();
        return true;
    }

    @Override
    @NotNull
    public String getCustomAnnounceFormat() {
        return this.customAnnounceFormat;
    }

    @Override
    public boolean setCustomAnnounceFormat(@NotNull final String format) {
        if (this.customAnnounceFormat.equals(format)) {
            return false;
        }

        this.customAnnounceFormat = format;
        this.update();
        return true;
    }

    @Override
    @NotNull
    public String getCustomBroadcastFormat() {
        return this.customBroadcastFormat;
    }

    @Override
    public boolean setCustomBroadcastFormat(@NotNull final String format) {
        if (this.customBroadcastFormat.equals(format)) {
            return false;
        }

        this.customBroadcastFormat = format;
        this.update();
        return true;
    }

    @Override
    @NotNull
    public String getCustomChannelFormat() {
        return this.customChannelFormat;
    }

    @Override
    public boolean setCustomChannelFormat(@NotNull final String format) {
        if (this.customChannelFormat.equals(format)) {
            return false;
        }

        this.customChannelFormat = format;
        this.update();
        return true;
    }

    // Methods for Chatter Collection of this Channel:
    @Override
    @NotNull
    public Collection<IChatter> getChatters() {
        return this.chatters;
    }

    @Override
    public boolean addChatter(@NotNull final IChatter chatter) {
        if (this.chatters.contains(chatter)) {
            return false;
        }

        //TODO: Announce
        this.chatters.add(chatter);

        if (!chatter.getChannels().contains(this)) {
            chatter.addChannel(this);
        }

        return true;
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.contains(chatter)) {
            return false;
        }

        this.chatters.remove(chatter);
        //TODO: Announce

        if (chatter.getChannels().contains(this)) {
            chatter.removeChannel(this);
        }

        return true;
    }

    // Methods for performing Actions:
    public void performAnnounce(@NotNull final String message) throws IllegalStateException {
        Chats instance = Chats.getInstance();
        if (instance == null) {
            throw new IllegalStateException("Method can only performed with a Server instance");
        }

        IFormatter formatter;

        if (this.isUseCustomFormat()) {
            formatter = new AnnounceFormatter(this, message, this.customAnnounceFormat);
        } else {
            formatter = new AnnounceFormatter(this, message, GlobalChannel.defaultAnnounceFormat);
        }

        if (instance.getSettingManager().isLogToConsole()) {
            instance.getLogger().fine(formatter.formatLog(instance.getSettingManager().isLogWithColor()));
        }

        String finalMsg = formatter.format();

        for (IChatter current : this.getChatters()) {
            current.getPlayer().sendMessage(finalMsg);
        }
    }

    public void performBroadcast(@NotNull final String message) throws IllegalStateException {
        Chats instance = Chats.getInstance();
        if (instance == null) {
            throw new IllegalStateException("Method can only performed with a Server instance");
        }

        IFormatter formatter;

        //TODO: Replace static Broadcast Prefix string with localed Broadcast Prefix string.
        if (this.isUseCustomFormat()) {
            formatter = new BroadcastFormatter(this, "Broadcast", message, this.customBroadcastFormat);
        } else {
            formatter = new BroadcastFormatter(this, "Broadcast", message, GlobalChannel.defaultBroadcastFormat);
        }

        if (instance.getSettingManager().isLogToConsole()) {
            instance.getLogger().fine(formatter.formatLog(instance.getSettingManager().isLogWithColor()));
        }

        String finalMsg = formatter.format();

        for (IChatter current : this.getChatters()) {
            current.getPlayer().sendMessage(finalMsg);
        }
    }

    @Override
    public void performChat(@NotNull final IChatter sender, @NotNull final String message)
            throws IllegalArgumentException, IllegalStateException {
        if (!this.chatters.contains(sender)) {
            throw new IllegalArgumentException("The sender must be in the channel: " + this.getFullName());
        }

        Chats instance = Chats.getInstance();
        if (instance == null) {
            throw new IllegalStateException("Method can only performed with a Server instance");
        }

        ChatterChatChannelEvent event;

        if (this.isUseCustomFormat()) {
            event = new ChatterChatChannelEvent(sender, this, message, this.customChannelFormat);
        } else {
            event = new ChatterChatChannelEvent(sender, this, message, GlobalChannel.defaultChannelFormat);
        }

        instance.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        IFormatter formatter = new ChannelFormatter(instance.getChatService(), event.getChannel(), event.getChatter(),
                event.getMessage(), event.getFormat());

        if (instance.getSettingManager().isLogToConsole()) {
            instance.getLogger().fine(formatter.formatLog(instance.getSettingManager().isLogWithColor()));
        }

        String finalMsg = formatter.format();

        if (this.hasDistance()) {
            Location senderLocation = event.getChatter().getPlayer().getLocation();
            String senderWorldName = event.getChatter().getPlayer().getWorld().getName();

            for (IChatter current : this.getChatters()) {
                if (current.getPlayer().getWorld().getName().equals(senderWorldName)) {
                    if (current.getPlayer().getLocation().distance(senderLocation) < this.distance) {
                        current.getPlayer().sendMessage(finalMsg);
                    }
                }
            }
        } else {
            if (this.isCrossWorld()) {
                for (IChatter current : this.getChatters()) {
                    current.getPlayer().sendMessage(finalMsg);
                }
            } else {
                String senderWorldName = event.getChatter().getPlayer().getWorld().getName();

                for (IChatter current : this.getChatters()) {
                    if (current.getPlayer().getWorld().getName().equals(senderWorldName)) {
                        current.getPlayer().sendMessage(finalMsg);
                    }
                }
            }
        }
    }

    // Method for Storage update:
    private void update() {
        if (this.isPersistChannel()) {
            this.storage.update(this);
        }
    }

    // Static Methods:
    @NotNull
    public static String getDefaultAnnounceFormat() {
        return GlobalChannel.defaultAnnounceFormat;
    }

    public static boolean setDefaultAnnounceFormat(@NotNull final String format) {
        if (GlobalChannel.defaultAnnounceFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultAnnounceFormat = format;
        return true;
    }

    @NotNull
    public static String getDefaultBroadcastFormat() {
        return GlobalChannel.defaultBroadcastFormat;
    }

    public static boolean setDefaultBroadcastFormat(@NotNull final String format) {
        if (GlobalChannel.defaultBroadcastFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultBroadcastFormat = format;
        return true;
    }

    @NotNull
    public static String getDefaultChannelFormat() {
        return GlobalChannel.defaultChannelFormat;
    }

    public static boolean setDefaultChannelFormat(@NotNull final String format) {
        if (GlobalChannel.defaultChannelFormat.equals(format)) {
            return false;
        }

        GlobalChannel.defaultChannelFormat = format;
        return true;
    }

    @NotNull
    public static ChatColor getDefaultChannelColor() {
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