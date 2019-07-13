package de.g4memas0n.Chats.formatters;

import org.jetbrains.annotations.NotNull;

public interface IFormatter {

    /**
     * Formats the format with the message of this Formatter and applies the Bukkit/Spigot ChatColor on it.
     * @return the formatted message.
     */
    @NotNull String format();

    /**
     * Formats the format with the message of this Formatter and applied the ANSIColor for the console on it when the
     * console should be colored.
     * @param colored if the formatted message should be colored.
     * @return the formatted message.
     */
    @NotNull String formatLog(final boolean colored);
}