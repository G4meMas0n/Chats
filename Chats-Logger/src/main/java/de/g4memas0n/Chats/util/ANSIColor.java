package de.g4memas0n.Chats.util;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.util.HashMap;
import java.util.Map;

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
    BOLD("[1m", ChatColor.BOLD),
    UNDERLINE("[4m", ChatColor.UNDERLINE),
    RESET("[0m", ChatColor.RESET);

    private static final String ANSI_COLOR_PREFIX = "\u001b";
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
        this.toString = ANSI_COLOR_PREFIX + code;
        this.bukkitColor = bukkitColor;
    }

    @NotNull
    public String getCode() {
        return this.code;
    }

    @NotNull
    public ChatColor getBukkitColor() {
        return this.bukkitColor;
    }

    @Override
    @NotNull
    public String toString() {
        return this.toString;
    }

    @NotNull
    public static ANSIColor getByBukkitColor(@NotNull final ChatColor bukkitColor) throws IllegalArgumentException {
        if (!BY_BUKKIT_COLOR.containsKey(bukkitColor)) {
            throw new IllegalArgumentException("Bukkit/Spigot ChatColor not supported!");
        }

        return BY_BUKKIT_COLOR.get(bukkitColor);
    }

    @NotNull
    public static String translateBukkitColor(@NotNull String input) {
        for (ChatColor current : ChatColor.values()) {
            if (input.contains(current.toString())) {
                try {
                    input = input.replaceAll(current.toString(), ANSIColor.getByBukkitColor(current).toString);
                } catch (IllegalArgumentException ex) {
                    input = input.replaceAll(current.toString(), "");
                }
            }
        }

        return input.concat(ANSIColor.RESET.toString);
    }

    @NotNull
    public static String stripColor(@NotNull final String input) {
        return input.replaceAll("\u001B\\[[;\\d]*m", "");
    }
}