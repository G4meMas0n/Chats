package de.g4memas0n.chats.util.logging;

import org.bukkit.ChatColor;
import org.fusesource.jansi.Ansi;
import org.fusesource.jansi.Ansi.Attribute;
import org.fusesource.jansi.Ansi.Color;
import org.fusesource.jansi.AnsiString;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * ANSI Codes Enum for all ANSI Codes that corresponds to a Bukkit chat color. Can be Used to colorize the console log.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 15th, 2020
 * changed: June 15th, 2020
 */
public enum ANSICode {

    // Normal Colors:
    BLACK(Ansi.ansi().reset().fg(Color.BLACK).boldOff().toString(), ChatColor.BLACK),
    BLUE(Ansi.ansi().reset().fg(Color.BLUE).boldOff().toString(), ChatColor.DARK_BLUE),
    GREEN(Ansi.ansi().reset().fg(Color.GREEN).boldOff().toString(), ChatColor.DARK_GREEN),
    CYAN(Ansi.ansi().reset().fg(Color.CYAN).boldOff().toString(), ChatColor.DARK_AQUA),
    RED(Ansi.ansi().reset().fg(Color.RED).boldOff().toString(), ChatColor.DARK_RED),
    MAGENTA(Ansi.ansi().reset().fg(Color.MAGENTA).boldOff().toString(), ChatColor.DARK_PURPLE),
    YELLOW(Ansi.ansi().reset().fg(Color.YELLOW).boldOff().toString(), ChatColor.GOLD),
    WHITE(Ansi.ansi().reset().fg(Color.WHITE).boldOff().toString(), ChatColor.GRAY),

    // Bright Colors:
    BRIGHT_BLACK(Ansi.ansi().reset().fg(Color.BLACK).bold().toString(), ChatColor.DARK_GRAY),
    BRIGHT_BLUE(Ansi.ansi().reset().fg(Color.BLUE).bold().toString(), ChatColor.BLUE),
    BRIGHT_GREEN(Ansi.ansi().reset().fg(Color.GREEN).bold().toString(), ChatColor.GREEN),
    BRIGHT_CYAN(Ansi.ansi().reset().fg(Color.CYAN).bold().toString(), ChatColor.AQUA),
    BRIGHT_RED(Ansi.ansi().reset().fg(Color.RED).bold().toString(), ChatColor.RED),
    BRIGHT_MAGENTA(Ansi.ansi().reset().fg(Color.MAGENTA).bold().toString(), ChatColor.LIGHT_PURPLE),
    BRIGHT_YELLOW(Ansi.ansi().reset().fg(Color.YELLOW).bold().toString(), ChatColor.YELLOW),
    BRIGHT_WHITE(Ansi.ansi().reset().fg(Color.WHITE).bold().toString(), ChatColor.WHITE),

    // Formats:
    BLINK_SLOW(Ansi.ansi().a(Attribute.BLINK_SLOW).toString(), ChatColor.MAGIC, true),
    UNDERLINE_DOUBLE(Ansi.ansi().a(Attribute.UNDERLINE_DOUBLE).toString(), ChatColor.BOLD, true),
    STRIKE_THROUGH_ON(Ansi.ansi().a(Attribute.STRIKETHROUGH_ON).toString(), ChatColor.STRIKETHROUGH, true),
    UNDERLINE(Ansi.ansi().a(Attribute.UNDERLINE).toString(), ChatColor.UNDERLINE, true),
    ITALIC(Ansi.ansi().a(Attribute.ITALIC).toString(), ChatColor.ITALIC, true),

    // Reset:
    RESET(Ansi.ansi().reset().toString(), ChatColor.RESET);

    /**
     * a map of all Bukkit chat colors with their associated ANSI color.
     */
    private static final Map<ChatColor, ANSICode> BY_BUKKIT_COLOR = new HashMap<>();

    private final ChatColor bukkitColor;
    private final String code;
    private final boolean format;

    static {
        for (ANSICode ansi : ANSICode.values()) {
            BY_BUKKIT_COLOR.put(ansi.getBukkitColor(), ansi);
        }
    }

    ANSICode(@NotNull final String code, @NotNull final ChatColor bukkitColor) {
        this.code = code;
        this.bukkitColor = bukkitColor;
        this.format = false;
    }

    ANSICode(@NotNull final String code, @NotNull final ChatColor bukkitColor, final boolean format) {
        this.code = code;
        this.bukkitColor = bukkitColor;
        this.format = format;
    }

    public @NotNull String getCode() {
        return this.code;
    }

    public @NotNull ChatColor getBukkitColor() {
        return this.bukkitColor;
    }

    public boolean isColor() {
        return !this.format;
    }

    public boolean isFormat() {
        return this.format && this != RESET;
    }

    @Override
    public @NotNull String toString() {
        return this.code;
    }

    @SuppressWarnings("unused")
    public static @Nullable ANSICode getByBukkitColor(@NotNull final ChatColor bukkitColor) {
        return BY_BUKKIT_COLOR.get(bukkitColor);
    }

    public static @NotNull String translateBukkitColor(@NotNull String input) {
        for (final ChatColor color : ChatColor.values()) {
            final ANSICode ansi = BY_BUKKIT_COLOR.get(color);

            if (ansi != null) {
                input = input.replaceAll("(?i)" + color.toString(), ansi.getCode());
            } else {
                input = input.replaceAll("(?i)" + color.toString(), "");
            }
        }

        return input + ANSICode.RESET.toString();
    }

    public static @NotNull String stripColor(@NotNull final String input) {
        return new AnsiString(input).getPlain().toString();
    }
}
