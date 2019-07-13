package de.g4memas0n.Chats.channels;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.events.ChatterChatConversionEvent;
import de.g4memas0n.Chats.formatters.ConversionFormatter;
import de.g4memas0n.Chats.formatters.IConversionFormatter;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashSet;
import java.util.Set;

public final class ConversionChannel implements IChannel {
    private static String conversionFormat = IChannel.DEFAULT_CONVERSION_FORMAT;
    private static String conversionTwitterFormat = IChannel.DEFAULT_CONVERSION_TWITTER_FORMAT;
    private static ChatColor conversionColor = ChatColor.LIGHT_PURPLE;
    private final String fullName;
    private final String shortName;
    private final Set<IChatter> chatters;

    public ConversionChannel(@NotNull final IChatter firstChatter,
                             @NotNull final IChatter secondChatter) throws IllegalArgumentException {
        this.fullName = ConversionChannel.buildFullName(firstChatter, secondChatter);
        this.shortName = ConversionChannel.buildShortName(firstChatter, secondChatter);
        this.chatters = new HashSet<>();
        this.chatters.add(firstChatter);
        this.chatters.add(secondChatter);
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
    public boolean setShortName(@NotNull final String shortName) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    @NotNull
    public ChatColor getChatColor() throws UnsupportedOperationException {
        return ConversionChannel.conversionColor;
    }

    @Override
    public boolean setChatColor(@NotNull final ChatColor chatColor) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    public boolean equals(@NotNull final Object object) {
        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final ConversionChannel channel = (ConversionChannel) object;
        return this.getChatters().equals(channel.getChatters());
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
    public boolean setCrossWorld(final boolean state) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
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
    public boolean setDistance(final int distance) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    public boolean hasPassword() {
        return false;
    }

    @Override
    @Nullable
    public String getPassword() {
        return null;
    }

    @Override
    public boolean setPassword(@NotNull final String password) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    public boolean removePassword() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not available in ConversionChannel!");
    }

    // Methods for Custom Channel Formats:
    @Override
    public boolean isUseCustomFormat() throws UnsupportedOperationException {
        return false;
    }

    @Override
    public boolean setUseCustomFormat(boolean state) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    @NotNull
    public String getCustomAnnounceFormat() {
        return "";
    }

    @Override
    public boolean setCustomAnnounceFormat(@NotNull String format) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    @NotNull
    public String getCustomBroadcastFormat() {
        return "";
    }

    @Override
    public boolean setCustomBroadcastFormat(@NotNull String format) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    @NotNull
    public String getCustomChannelFormat() {
        return "";
    }

    @Override
    public boolean setCustomChannelFormat(@NotNull String format) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    // Methods for Chatter Collection of this Channel:
    @Override
    @NotNull
    public Set<IChatter> getChatters() {
        return this.chatters;
    }

    @Override
    public boolean addChatter(@NotNull final IChatter chatter) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    @Override
    public boolean removeChatter(@NotNull final IChatter chatter) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Not allowed in ConversionChannel!");
    }

    // Method for performing Chats:
    @Override
    public void performChat(@NotNull final IChatter sender,
                            @NotNull final String message) throws IllegalArgumentException, IllegalStateException {
        if (!this.chatters.contains(sender)) {
            throw new IllegalArgumentException("The sender: " + sender.getPlayer().getName()
                    + " must be in the conversion channel: " + this.getFullName());
        }

        if (this.chatters.size() != 2) {
            throw new IllegalStateException("There are too few or too many chatters");
        }

        for (IChatter current : this.chatters) {
            if (!current.equals(sender)) {
                ConversionChannel.performConversion(sender, current, message);
                return;
            }
        }
    }

    // Static Methods:
    public static void performConversion(@NotNull final IChatter sender,
                                         @NotNull final IChatter partner,
                                         @NotNull final String message) throws IllegalStateException {
        Chats instance = Chats.getInstance();
        if (instance == null) {
            throw new IllegalStateException("Method can only performed with a Server instance");
        }

        ChatterChatConversionEvent event = new ChatterChatConversionEvent(sender, partner, message);

        instance.getPluginManager().callEvent(event);

        if (event.isCancelled()) {
            return;
        }

        IConversionFormatter formatter = new ConversionFormatter(instance.getChatService(), event.getChatter(),
                event.getPartner(), event.getMessage(), ConversionChannel.conversionFormat,
                ConversionChannel.conversionTwitterFormat, ConversionChannel.conversionColor);

        if (instance.getSettingManager().isLogToConsole()) {
            instance.getLogger().fine(formatter.formatLog(instance.getSettingManager().isLogWithColor()));
        }

        if (instance.getSettingManager().isUseConversionTwitterFormat()) {
            //TODO: Replace static Address string with localed Address string.
            sender.getPlayer().sendMessage(formatter.formatTwitterStyle(partner, "To"));
            partner.getPlayer().sendMessage(formatter.formatTwitterStyle(sender, "From"));
        } else {
            String finalMsg = formatter.format();
            sender.getPlayer().sendMessage(finalMsg);
            partner.getPlayer().sendMessage(finalMsg);
        }

        sender.setLastConversionPartner(partner);
        partner.setLastConversionPartner(sender);
    }

    @NotNull
    public static String getConversionFormat() {
        return ConversionChannel.conversionFormat;
    }

    public static boolean setConversionFormat(@NotNull final String format) {
        if (ConversionChannel.conversionFormat.equals(format)) {
            return false;
        }

        ConversionChannel.conversionFormat = format;
        return true;
    }

    @NotNull
    public static String getConversionTwitterFormat() {
        return ConversionChannel.conversionTwitterFormat;
    }

    public static boolean setConversionTwitterFormat(@NotNull final String format) {
        if (ConversionChannel.conversionTwitterFormat.equals(format)) {
            return false;
        }

        ConversionChannel.conversionTwitterFormat = format;
        return true;
    }

    @NotNull
    public static ChatColor getConversionColor() {
        return ConversionChannel.conversionColor;
    }

    public static boolean setConversionColor(@NotNull final ChatColor color) {
        if (ConversionChannel.conversionColor == color) {
            return false;
        }

        ConversionChannel.conversionColor = color;
        return true;
    }

    @NotNull
    public static String buildFullName(@NotNull final IChatter firstChatter,
                                       @NotNull final IChatter secondChatter) throws IllegalArgumentException {
        final String firstUUID = firstChatter.getPlayer().getUniqueId().toString();
        final String secondUUID = secondChatter.getPlayer().getUniqueId().toString();

        final int compared = firstUUID.compareTo(secondUUID);

        if (compared < 0) {
            return firstUUID + "_" + secondUUID;
        } else if (compared > 0) {
            return secondUUID + "_" + firstUUID;
        } else {
            throw new IllegalArgumentException("Both chatters has the same UUID! Chatters must be different.");
        }
    }

    @NotNull
    public static String buildShortName(@NotNull final IChatter firstChatter,
                                        @NotNull final IChatter secondChatter) throws IllegalArgumentException {
        final String firstName = firstChatter.getPlayer().getName();
        final String secondName = secondChatter.getPlayer().getName();

        final int compared = firstName.compareTo(secondName);

        if (compared < 0) {
            return firstName + "_" + secondName;
        } else if (compared > 0) {
            return secondName + "_" + firstName;
        } else {
            throw new IllegalArgumentException("Both chatters has the same name! Chatters must be different.");
        }
    }
}