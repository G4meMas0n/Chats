package de.g4memas0n.Chats.util;

import org.apache.commons.lang.StringUtils;
import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;

public class InputUtil {

    public static final String BOOLEAN_TRUE = "true";
    public static final String BOOLEAN_FALSE = "false";

    private InputUtil() { }

    public static int parseInt(@NotNull final String number) throws IllegalArgumentException {
        try {
            return Integer.parseInt(number);
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(number + " is not a integer");
        }
    }

    public static boolean isInt(@NotNull final String number) {
        try {
            Integer.parseInt(number);
            return true;
        } catch (NumberFormatException ex) {
            return false;
        }
    }

    public static boolean parseBoolean(@NotNull final String bool) throws IllegalArgumentException {
        if (bool.equalsIgnoreCase(BOOLEAN_TRUE)) {
            return true;
        } else if (bool.equalsIgnoreCase(BOOLEAN_FALSE)) {
            return false;
        }

        throw new IllegalArgumentException(bool + " is not a boolean value");
    }

    public static @NotNull ChatColor parseChatColor(@NotNull final String color) throws IllegalArgumentException {
        try {
            return ChatColor.valueOf(color);
        } catch (IllegalArgumentException ex) {
            for (final ChatColor current : ChatColor.values()) {
                if (current.name().equalsIgnoreCase(color)) {
                    return current;
                }
            }

            throw new IllegalArgumentException(color + " is not a chat color");
        }
    }

    public static boolean containsInput(@NotNull final String string, @NotNull final String input) {
        return StringUtils.containsIgnoreCase(string, input);
    }
}
