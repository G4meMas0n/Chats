package de.g4memas0n.Chats.formatters;

import de.g4memas0n.Chats.chatters.IChatter;
import org.jetbrains.annotations.NotNull;

public interface IConversionFormatter extends IFormatter {

    /**
     * Formats the format with the message of this Formatter in an another style and applies the Bukkit/Spigot
     * ChatColor on it.
     * @param partner the partner that is addressed.
     * @param address the address of this style.
     * @return the formatted message.
     */
    @NotNull String formatTwitterStyle(@NotNull final IChatter partner,
                                       @NotNull final String address);
}