package de.g4memas0n.Chats.channel;

import de.g4memas0n.Chats.chatter.IChatter;
import de.g4memas0n.Chats.util.Placeholder;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Representation of the conversion formatter, implements the {@link IConversionFormatter} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 7th, 2019
 * last change: September 11th, 2019
 */
public final class ConversionFormatter implements IConversionFormatter {

    /**
     * The empty string that will be returned when the channel stored in this formatter do not support an feature.
     */
    private static final String EMPTY_STRING = "";

    private String preAnnounceFormat;
    private String preChannelFormat;
    private String preFromFormat;
    private String preToFormat;

    protected ConversionFormatter() {
        this.update();
    }

    @Override
    public void update() throws IllegalArgumentException {
        this.preAnnounceFormat = this.prepareAnnounceFormat(ConversionChannel.getDefaultAnnounceFormat());
        this.preChannelFormat = this.prepareChannelFormat(ConversionChannel.getDefaultChannelFormat());

        final String twitterStyleFormat = ConversionChannel.getTwitterStyleFormat();
        this.preFromFormat = this.prepareFromFormat(twitterStyleFormat);
        this.preToFormat = this.prepareToFormat(twitterStyleFormat);
    }

    @Override
    public boolean isAnnounceLoggable() {
        return false;
    }

    @Override
    public @NotNull String formatAnnounce(@NotNull final String message) {
        String formatted = this.preAnnounceFormat;

        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public boolean isBroadcastLoggable() {
        return false;
    }

    @Override
    public @NotNull String formatBroadcast(@NotNull final String message) {
        return EMPTY_STRING;
    }

    @Override
    public boolean isChatLoggable() {
        return false;
    }

    @Override
    public @NotNull String formatChat(@NotNull final IChatter sender,
                                      @NotNull final String message) {
        return EMPTY_STRING;
    }

    @Override
    public @NotNull String formatChat(@NotNull final IChatter sender,
                                      @NotNull final Set<IChatter> partners,
                                      @NotNull final String message) {
        String formatted = this.preChannelFormat;

        formatted = formatted.replaceFirst(Placeholder.CON_SENDER.toString(), sender.getPlayer().getDisplayName());
        formatted = formatted.replaceFirst(Placeholder.CON_PARTNER.toString(), this.joinNames(partners));
        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public @NotNull String formatFrom(@NotNull final IChatter sender,
                                      @NotNull final String message) {
        String formatted = this.preFromFormat;

        formatted = formatted.replaceFirst(Placeholder.CON_PARTNER.toString(), sender.getPlayer().getDisplayName());
        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    @Override
    public @NotNull String formatTo(@NotNull final Set<IChatter> partners,
                                    @NotNull final String message) {
        String formatted = this.preToFormat;

        formatted = formatted.replaceFirst(Placeholder.CON_PARTNER.toString(), this.joinNames(partners));
        formatted = formatted.replaceFirst(Placeholder.MESSAGE.toString(), message);

        return Placeholder.stripPlaceholders(formatted);
    }

    /**
     * Joins the given set of chatters to one string containing all chatter display names separated by an comma.
     * @param chatters the set of chatters that should be joined to one string.
     * @return the joined string.
     */
    private @NotNull String joinNames(@NotNull final Set<IChatter> chatters) {
        final List<String> chatterNames = new ArrayList<>();

        for (IChatter current : chatters) {
            chatterNames.add(current.getPlayer().getDisplayName());
        }

        return String.join(", ", chatterNames);
    }

    /**
     * Prepares the given announce format with the information of the conversion channel.
     * @param format the announce format that should be prepared.
     * @return the prepared announce format.
     */
    private @NotNull String prepareAnnounceFormat(@NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                ConversionChannel.getDefaultChannelColor().toString());

        return format;
    }

    /**
     * Prepares the given channel format with the information of the conversion channel.
     * @param format the channel format that should be prepared.
     * @return the prepared channel format.
     */
    private @NotNull String prepareChannelFormat(@NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                ConversionChannel.getDefaultChannelColor().toString());

        return format;
    }

    /**
     * Prepares the given twitter-style format with the information of the conversion channel.
     * @param format the twitter-style format that should be prepared.
     * @return the prepared twitter-style-from format.
     */
    private @NotNull String prepareFromFormat(@NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                ConversionChannel.getDefaultChannelColor().toString());
        //TODO: Use Resource Bundle for getting correct conversion address from prefix
        format = format.replaceFirst(Placeholder.CON_ADDRESS.toString(), "From");

        return format;
    }

    /**
     * Prepares the given twitter-style format with the information of the conversion channel.
     * @param format the twitter-style format that should be prepared.
     * @return the prepared twitter-style-to format.
     */
    private @NotNull String prepareToFormat(@NotNull String format) {
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                ConversionChannel.getDefaultChannelColor().toString());
        //TODO: Use Resource Bundle for getting correct conversion address to prefix
        format = format.replaceFirst(Placeholder.CON_ADDRESS.toString(), "To");

        return format;
    }
}
