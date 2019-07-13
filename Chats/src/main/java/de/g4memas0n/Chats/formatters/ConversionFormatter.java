package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.chatters.IChatter;
import de.g4memas0n.Chats.util.ANSIColor;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.ChatColor;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public final class ConversionFormatter implements IConversionFormatter {
    private static final int DISPLAY_NAME = 0;
    private static final int PLAIN_NAME = 1;
    private static final int WORLD = 2;
    private static final int PREFIX = 3;
    private static final int SUFFIX = 4;
    private static final int GROUP = 5;
    private static final int GROUP_PREFIX = 6;
    private static final int GROUP_SUFFIX = 7;

    private final Map<UUID, Map<Integer, String>> chattersData;
    private final Chat chatService;
    private final String message;
    private final String conversionFormat;
    private final String conversionTwitterFormat;
    private final ChatColor conversionColor;

    public ConversionFormatter(@Nullable final Chat chatService,
                               @NotNull final IChatter sender,
                               @NotNull final IChatter partner,
                               @NotNull final String message,
                               @NotNull final String format,
                               @NotNull final String twitterFormat,
                               @NotNull final ChatColor chatColor) {
        this.chatService = chatService;

        this.chattersData = new HashMap<>();
        this.chattersData.put(sender.getPlayer().getUniqueId(), this.getChatterMap(sender));
        this.chattersData.put(partner.getPlayer().getUniqueId(), this.getChatterMap(partner));

        this.message = message;
        this.conversionFormat = this.formatMessage(sender, partner, format);
        this.conversionTwitterFormat = this.formatTwitterMessage(twitterFormat);
        this.conversionColor = chatColor;
    }

    @NotNull
    private String formatMessage(@NotNull final IChatter sender,
                                 @NotNull final IChatter partner,
                                 @NotNull String format) throws IllegalArgumentException {
        if (!this.chattersData.containsKey(sender.getPlayer().getUniqueId())
                || !this.chattersData.containsKey(partner.getPlayer().getUniqueId())) {
            throw new IllegalArgumentException("Formatter don't contains one or both Chatters");
        }

        Map<Integer, String> senderData = this.chattersData.get(sender.getPlayer().getUniqueId());
        Map<Integer, String> partnerData = this.chattersData.get(partner.getPlayer().getUniqueId());

        format = format.replaceAll(Placeholder.MESSAGE.toString(), this.message);

        format = format.replaceAll(Placeholder.SENDER.toString(), senderData.get(DISPLAY_NAME));
        format = format.replaceAll(Placeholder.SENDER_PLAIN.toString(), senderData.get(PLAIN_NAME));
        format = format.replaceAll(Placeholder.SENDER_WORLD.toString(), senderData.get(WORLD));
        format = format.replaceAll(Placeholder.SENDER_PREFIX.toString(), senderData.get(PREFIX));
        format = format.replaceAll(Placeholder.SENDER_SUFFIX.toString(), senderData.get(SUFFIX));
        format = format.replaceAll(Placeholder.SENDER_GROUP.toString(), senderData.get(GROUP));
        format = format.replaceAll(Placeholder.SENDER_GROUP_PREFIX.toString(), senderData.get(GROUP_PREFIX));
        format = format.replaceAll(Placeholder.SENDER_GROUP_SUFFIX.toString(), senderData.get(GROUP_SUFFIX));

        format = format.replaceAll(Placeholder.CON_PARTNER.toString(), partnerData.get(DISPLAY_NAME));
        format = format.replaceAll(Placeholder.CON_PARTNER_PLAIN.toString(), partnerData.get(PLAIN_NAME));
        format = format.replaceAll(Placeholder.CON_PARTNER_WORLD.toString(), partnerData.get(WORLD));
        format = format.replaceAll(Placeholder.CON_PARTNER_PREFIX.toString(), partnerData.get(PREFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_SUFFIX.toString(), partnerData.get(SUFFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP.toString(), partnerData.get(GROUP));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP_PREFIX.toString(), partnerData.get(GROUP_PREFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP_SUFFIX.toString(), partnerData.get(GROUP_SUFFIX));

        return format;
    }

    @NotNull
    private String formatTwitterMessage(@NotNull String format) {
        format = format.replaceAll(Placeholder.MESSAGE.toString(), this.message);
        format = format.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.conversionColor.toString());

        return format;
    }

    @Override
    @NotNull
    public String format() {
        return this.conversionFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), this.conversionColor.toString());
    }

    @Override
    @NotNull
    public String formatLog(final boolean colored) {
        if (colored) {
            return this.conversionFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(),
                    ANSIColor.getByBukkitColor(this.conversionColor).toString()) + ANSIColor.RESET.toString();
        } else {
            return this.conversionFormat.replaceAll(Placeholder.CHANNEL_COLOR.toString(), "");
        }
    }

    @Override
    @NotNull
    public String formatTwitterStyle(@NotNull final IChatter partner,
                                     @NotNull final String address) {
        if (!this.chattersData.containsKey(partner.getPlayer().getUniqueId())) {
            throw new IllegalArgumentException("Formatter don't contains this chatter");
        }

        Map<Integer, String> chatterData = this.chattersData.get(partner.getPlayer().getUniqueId());
        String format = this.conversionTwitterFormat;

        format = format.replaceAll(Placeholder.CON_ADDRESS.toString(), address);

        format = format.replaceAll(Placeholder.CON_PARTNER.toString(), chatterData.get(DISPLAY_NAME));
        format = format.replaceAll(Placeholder.CON_PARTNER_PLAIN.toString(), chatterData.get(PLAIN_NAME));
        format = format.replaceAll(Placeholder.CON_PARTNER_WORLD.toString(), chatterData.get(WORLD));
        format = format.replaceAll(Placeholder.CON_PARTNER_PREFIX.toString(), chatterData.get(PREFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_SUFFIX.toString(), chatterData.get(SUFFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP.toString(), chatterData.get(GROUP));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP_PREFIX.toString(), chatterData.get(GROUP_PREFIX));
        format = format.replaceAll(Placeholder.CON_PARTNER_GROUP_SUFFIX.toString(), chatterData.get(GROUP_SUFFIX));

        return format;
    }

    @NotNull
    private Map<Integer, String> getChatterMap(@NotNull final IChatter chatter) {
        Map<Integer, String> chatterData = new HashMap<>();
        Player chattersPlayer = chatter.getPlayer();
        World chattersWorld = chattersPlayer.getWorld();

        chatterData.put(DISPLAY_NAME, chattersPlayer.getDisplayName());
        chatterData.put(PLAIN_NAME, chattersPlayer.getName());
        chatterData.put(WORLD, chattersWorld.getName());

        if (this.chatService != null) {
            chatterData.put(PREFIX, chatService.getPlayerPrefix(chattersPlayer));
            chatterData.put(SUFFIX, chatService.getPlayerSuffix(chattersPlayer));

            String chattersGroup = this.chatService.getPrimaryGroup(chattersPlayer);
            chatterData.put(GROUP, chattersGroup);
            chatterData.put(GROUP_PREFIX, this.chatService.getGroupPrefix(chattersWorld, chattersGroup));
            chatterData.put(GROUP_SUFFIX, this.chatService.getGroupSuffix(chattersWorld, chattersGroup));
        } else {
            chatterData.put(PREFIX, "");
            chatterData.put(SUFFIX, "");
            chatterData.put(GROUP, "");
            chatterData.put(GROUP_PREFIX, "");
            chatterData.put(GROUP_SUFFIX, "");
        }

        return chatterData;
    }
}