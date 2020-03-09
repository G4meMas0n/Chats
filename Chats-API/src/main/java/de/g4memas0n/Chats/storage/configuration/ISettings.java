package de.g4memas0n.Chats.storage.configuration;

import de.g4memas0n.Chats.storage.IStorageHolder;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.util.Locale;

public interface ISettings extends IStorageHolder {

    @NotNull String getAnnounceFormat();

    @NotNull String getBroadcastFormat();

    @NotNull String getChatFormat();

    @NotNull String getConversationFormat();

    @NotNull String getDefaultChannel();

    void setDefaultChannel(@NotNull final String channel);

    @NotNull ChatColor getChannelColor();

    @NotNull ChatColor getConversationColor();

    @NotNull Locale getLocale();

    boolean isLogChat();

    boolean isLogColored();

    boolean isLogDebug();

    boolean isLogToConsole();

    boolean isLogToFile();
}
