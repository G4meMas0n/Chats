package de.g4memas0n.Chats.util;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * ANSI Color Enum for all ANSI Colors that corresponds to a Bukkit chat color. Can be Used to colorize the console log.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: July 10th, 2019
 * changed: January 2nd, 2020
 */
public enum ANSIColor {
    BLACK("[30m", ChatColor.BLACK),
    BLUE("[34m", ChatColor.DARK_BLUE),
    GREEN("[32m", ChatColor.DARK_GREEN),
    CYAN("[36m", ChatColor.DARK_AQUA),
    RED("[31m", ChatColor.DARK_RED),
    MAGENTA("[35m", ChatColor.DARK_PURPLE),
    YELLOW("[33m", ChatColor.GOLD),
    WHITE("[37m", ChatColor.GRAY),
    BRIGHT_BLACK("[30;1m", ChatColor.DARK_GRAY),
    BRIGHT_BLUE("[34;1m", ChatColor.BLUE),
    BRIGHT_GREEN("[32;1m", ChatColor.GREEN),
    BRIGHT_CYAN("[36;1m", ChatColor.AQUA),
    BRIGHT_RED("[31;1m", ChatColor.RED),
    BRIGHT_MAGENTA("[35;1m", ChatColor.LIGHT_PURPLE),
    BRIGHT_YELLOW("[33;1m", ChatColor.YELLOW),
    BRIGHT_WHITE("[37;1m", ChatColor.WHITE),
    RESET("[0m", ChatColor.RESET);

    /**
     * the prefix for all ANSI Colors.
     */
    private static final String ANSI_CODE_PREFIX = "\u001b";

    /**
     * a map of all Bukkit chat colors with their associated ANSI color.
     */
    private static final Map<ChatColor, ANSIColor> BY_BUKKIT_COLOR = new HashMap<>();

    private final String code;
    private final String toString;
    private final ChatColor bukkitColor;

    static {
        for (ANSIColor current : ANSIColor.values()) {
            BY_BUKKIT_COLOR.put(current.getBukkitColor(), current);
        }
    }

    ANSIColor(@NotNull final String code, @NotNull final ChatColor bukkitColor) {
        this.code = code;
        this.toString = ANSI_CODE_PREFIX + code;
        this.bukkitColor = bukkitColor;
    }

    public @NotNull String getCode() {
        return this.code;
    }

    public @NotNull ChatColor getBukkitColor() {
        return this.bukkitColor;
    }

    @Override
    public @NotNull String toString() {
        return this.toString;
    }

    public static @Nullable ANSIColor getByBukkitColor(@NotNull final ChatColor bukkitColor) {
        return BY_BUKKIT_COLOR.get(bukkitColor);
    }

    public static @NotNull String translateBukkitColor(@NotNull String input) {
        for (ANSIColor current : ANSIColor.values()) {
            if (!input.contains(current.getBukkitColor().toString())) {
                continue;
            }

            input = input.replace(current.getBukkitColor().toString(), current.toString());
        }

        return ChatColor.stripColor(input) + ANSIColor.RESET;
    }

    public static @NotNull String stripColor(@NotNull final String input) {
        return input.replaceAll("\u001B\\[[;\\d]*m", "");
    }
}
