package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.exception.UnsupportedFeatureException;
import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.event.ChatterChatConversionEvent;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Representation of a conversion channel, implements the {@link IChannel} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 13th, 2019
 * last change: October 1st, 2019
 */
public final class ConversionChannel implements IChannel {

    /**
     * the empty string. Used to return a non null string for unsupported formats.
     */
    private static final String EMPTY_STRING = "";

    /**
     * the conversion prefix. Used to detect to which channel a log belongs.
     */
    private static final String CONVERSION_PREFIX = "[CONVERSION]";

    /**
     * the default announce format. Used for all conversion channel announces.
     */
    private static String announceFormat = "{color}{message}";

    /**
     * the default conversion format. Used for all conversions and conversion-logs.
     */
    private static String channelFormat = "{color}[{sender}{color} -> {con-partner}{color}] {message}";

    /**
     * the twitter-style conversion format. Used for all conversion when the use-twitter-style option is enabled.
     */
    private static String twitterStyleFormat = "{color}[{con-address} {con-partner}{color}] {message}";

    /**
     * the default conversion chat color. Used for all conversions.
     */
    private static ChatColor channelColor = ChatColor.LIGHT_PURPLE;

    /**
     * the use-twitter-style option. Used to decide which format will be used for the conversion.
     */
    private static boolean useTwitterStyle = true;

    /**
     * the default conversion formatter. Used to format all conversions.
     */
    private static IConversionFormatter formatter = new ConversionFormatter();

    private final IChannelManager manager;

    private String fullName;
    private Set<IChatter> chatters;

    public ConversionChannel(@NotNull final IChannelManager manager,
                             @NotNull final IChatter... chatters) {
        this.manager = manager;
        this.chatters = new HashSet<>();

        Collections.addAll(this.chatters, chatters);

        this.fullName = ConversionChannel.buildName(this.chatters);
    }

    public ConversionChannel(@NotNull final IChannelManager manager,
                             @NotNull final Set<IChatter> chatters) {
        this.manager = manager;
        this.chatters = new HashSet<>(chatters);
        this.fullName = ConversionChannel.buildName(this.chatters);
    }

    // Methods for Channel Properties:
    @Override
    public @NotNull String getFullName() {
        return this.fullName;
    }

    @Override
    public @NotNull String getShortName() {
        return this.fullName;
    }

    @Override
    public boolean setShortName(@Nullable final String shortName) {
        return false;
    }

    @Override
    public @NotNull ChatColor getChatColor() {
        return channelColor;
    }

    @Override
    public boolean setChatColor(@NotNull final ChatColor chatColor) {
        return false;
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

        final IChannel channel = (ConversionChannel) object;
        return this.fullName.equals(channel.getFullName());
    }

    @Override
    public int hashCode() {
        final int prime = 53;
        int result = 3;

        result = prime * result + this.fullName.hashCode();

        return result;
    }

    // Methods for Channel Types:
    @Override
    public boolean isPersistChannel() {
        return false;
    }

    @Override
    public boolean isGlobalChannel() {
        return false;
    }

    @Override
    public boolean isConversionChannel() {
        return true;
    }

    // Methods for Channel Settings:
    @Override
    public boolean isCrossWorld() {
        return true;
    }

    @Override
    public boolean setCrossWorld(final boolean state) {
        return false;
    }

    @Override
    public boolean hasDistance() {
        return false;
    }

    @Override
    public int getDistance() {
        return -1;
    }

    @Override
    public boolean setDistance(final int distance) {
        return false;
    }

    @Override
    public boolean hasPassword() {
        return false;
    }

    @Override
    public @Nullable String getPassword() {
        return null;
    }

    @Override
    public boolean setPassword(@Nullable final String password) {
        return false;
    }

    // Methods for Channel Formatting:
    @Override
    public @NotNull IConversionFormatter getFormatter() {
        return formatter;
    }

    @Override
    public @NotNull String getAnnounceFormat() {
        return announceFormat;
    }

    @Override
    public boolean setAnnounceFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public @NotNull String getBroadcastFormat() {
        return EMPTY_STRING;
    }

    @Override
    public boolean setBroadcastFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public @NotNull String getChannelFormat() {
        return useTwitterStyle ? twitterStyleFormat : channelFormat;
    }

    @Override
    public boolean setChannelFormat(@Nullable final String format) {
        return false;
    }

    @Override
    public boolean isUseCustomFormat() {
        return false;
    }

    @Override
    public boolean setUseCustomFormat(final boolean state) {
        return false;
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
        this.fullName = buildName(this.chatters);
        return true;
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) {
        if (!this.chatters.contains(chatter)) {
            return false;
        }

        this.chatters.remove(chatter);

        if (this.chatters.size() < 2) {
            this.manager.removeConversionChannel(this.fullName);
            return true;
        }

        this.fullName = buildName(this.chatters);
        return true;
    }

    // Method for performing Chats:
    @Override
    public void performAnnounce(@NotNull final String message) {
        final String output = formatter.formatAnnounce(message);

        for (IChatter current : this.chatters) {
            current.getPlayer().sendMessage(output);
        }
    }

    @Override
    public void performBroadcast(@NotNull final String message) throws UnsupportedFeatureException {
        throw new UnsupportedFeatureException("Broadcasts not available for conversions!");
    }

    @Override
    public void performChat(@NotNull final IChatter sender,
                            @NotNull final String message) throws IllegalArgumentException, IllegalStateException {
        if (!this.chatters.contains(sender)) {
            throw new IllegalArgumentException("Sender: " + sender.getPlayer().getName()
                    + " must be a member of the conversion channel: " + this.getFullName());
        }

        Set<IChatter> partners = new HashSet<>(this.chatters);
        partners.remove(sender);

        performConversion(sender, partners, message);
    }

    // Static Methods:
    public static void performConversion(@NotNull final IChatter sender,
                                         @NotNull final Set<IChatter> partners,
                                         @NotNull final String message) {
        IChats instance = Chats.getInstance();

        ChatterChatConversionEvent event = new ChatterChatConversionEvent(sender, partners, message);

        if (instance != null) {
            instance.getPluginManager().callEvent(event);
        }

        if (event.isCancelled()) {
            return;
        }

        final String output = formatter.formatChat(sender, partners, event.getMessage());

        if (instance != null) {
            String log = output;

            if (!formatter.isChatLoggable()) {
                log = CONVERSION_PREFIX + log;
            }

            instance.logChat(log);
        }

        final Set<IChatter> allChatters = new HashSet<>(partners);
        allChatters.add(sender);

        if (useTwitterStyle) {
            final String outputTo = formatter.formatTo(partners, event.getMessage());
            final String outputFrom = formatter.formatFrom(sender, event.getMessage());

            sender.getPlayer().sendMessage(outputTo);
            sender.setLastConversionPartner(partners);

            for (IChatter current : partners) {
                current.getPlayer().sendMessage(outputFrom);
                current.setLastConversionPartner(allChatters);
            }
        } else {
            for (IChatter current : allChatters) {
                current.getPlayer().sendMessage(output);
                current.setLastConversionPartner(allChatters);
            }
        }
    }

    public static @NotNull String getDefaultAnnounceFormat() {
        return announceFormat;
    }

    public static boolean setDefaultAnnounceFormat(@NotNull final String format) {
        if (announceFormat.equals(format)) {
            return false;
        }

        announceFormat = format;
        return true;
    }

    public static @NotNull String getDefaultChannelFormat() {
        return channelFormat;
    }

    public static boolean setDefaultChannelFormat(@NotNull final String format) {
        if (channelFormat.equals(format)) {
            return false;
        }

        channelFormat = format;
        return true;
    }

    public static @NotNull ChatColor getDefaultChannelColor() {
        return channelColor;
    }

    public static boolean setDefaultChannelColor(@NotNull final ChatColor color) {
        if (channelColor == color) {
            return false;
        }

        channelColor = color;
        return true;
    }

    public static @NotNull String getTwitterStyleFormat() {
        return twitterStyleFormat;
    }

    public static boolean setTwitterStyleFormat(@NotNull final String format) {
        if (twitterStyleFormat.equals(format)) {
            return false;
        }

        twitterStyleFormat = format;
        return true;
    }

    public static boolean setUseTwitterFormat(final boolean enabled) {
        if (useTwitterStyle == enabled) {
            return false;
        }

        useTwitterStyle = enabled;
        return true;
    }

    public static @NotNull String buildName(@NotNull final Set<IChatter> chatters) {
        final List<String> chatterUUIDs = new ArrayList<>();

        for (IChatter current : chatters) {
            chatterUUIDs.add(current.getPlayer().getUniqueId().toString());
        }

        Collections.sort(chatterUUIDs);

        return String.join("_", chatterUUIDs);
    }
}
