package de.g4memas0n.Chats.util;

import org.bukkit.ChatColor;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public final class ANSIColorTest {

    @Test
    public void translateBukkitColorTest() {
        final String input = ChatColor.DARK_RED + "Hello" + ChatColor.DARK_GREEN + "World";
        final String output = ANSIColor.RED + "Hello" + ANSIColor.GREEN + "World" + ANSIColor.RESET;

        assertEquals(output, ANSIColor.translateBukkitColor(input));
    }

    @Test
    public void translateUnsupportedBukkitColorTest() {
        final String input = ChatColor.DARK_RED + "Hello" + ChatColor.MAGIC + "World";
        final String output = ANSIColor.RED + "HelloWorld" + ANSIColor.RESET;

        assertEquals(output, ANSIColor.translateBukkitColor(input));
    }

    @Test
    public void stripEightColorTest() {
        final String input = ANSIColor.RED + "Hello" + ANSIColor.GREEN + "World" + ANSIColor.RESET;
        final String output = "HelloWorld";

        assertEquals(output, ANSIColor.stripColor(input));
    }

    @Test
    public void stripSixteenColorTest() {
        final String input = ANSIColor.BRIGHT_RED + "Hello" + ANSIColor.BRIGHT_GREEN + "World" + ANSIColor.RESET;
        final String output = "HelloWorld";

        assertEquals(output, ANSIColor.stripColor(input));
    }

    @Test
    public void stripWithoutColorTest() {
        final String output = "HelloWorld";

        assertEquals(output, ANSIColor.stripColor(output));
    }

    @Test
    public void getByBukkitColorTest() {
        assertEquals(ANSIColor.BLACK, ANSIColor.getByBukkitColor(ChatColor.BLACK));
        assertEquals(ANSIColor.BLUE, ANSIColor.getByBukkitColor(ChatColor.DARK_BLUE));
        assertEquals(ANSIColor.GREEN, ANSIColor.getByBukkitColor(ChatColor.DARK_GREEN));
        assertEquals(ANSIColor.CYAN, ANSIColor.getByBukkitColor(ChatColor.DARK_AQUA));
        assertEquals(ANSIColor.RED, ANSIColor.getByBukkitColor(ChatColor.DARK_RED));
        assertEquals(ANSIColor.MAGENTA, ANSIColor.getByBukkitColor(ChatColor.DARK_PURPLE));
        assertEquals(ANSIColor.YELLOW, ANSIColor.getByBukkitColor(ChatColor.GOLD));
        assertEquals(ANSIColor.WHITE, ANSIColor.getByBukkitColor(ChatColor.GRAY));
        assertEquals(ANSIColor.BRIGHT_BLACK, ANSIColor.getByBukkitColor(ChatColor.DARK_GRAY));
        assertEquals(ANSIColor.BRIGHT_BLUE, ANSIColor.getByBukkitColor(ChatColor.BLUE));
        assertEquals(ANSIColor.BRIGHT_GREEN, ANSIColor.getByBukkitColor(ChatColor.GREEN));
        assertEquals(ANSIColor.BRIGHT_CYAN, ANSIColor.getByBukkitColor(ChatColor.AQUA));
        assertEquals(ANSIColor.BRIGHT_RED, ANSIColor.getByBukkitColor(ChatColor.RED));
        assertEquals(ANSIColor.BRIGHT_MAGENTA, ANSIColor.getByBukkitColor(ChatColor.LIGHT_PURPLE));
        assertEquals(ANSIColor.BRIGHT_YELLOW, ANSIColor.getByBukkitColor(ChatColor.YELLOW));
        assertEquals(ANSIColor.BRIGHT_WHITE, ANSIColor.getByBukkitColor(ChatColor.WHITE));
        assertEquals(ANSIColor.BOLD, ANSIColor.getByBukkitColor(ChatColor.BOLD));
        assertEquals(ANSIColor.UNDERLINE, ANSIColor.getByBukkitColor(ChatColor.UNDERLINE));
        assertEquals(ANSIColor.RESET, ANSIColor.getByBukkitColor(ChatColor.RESET));
    }

    @Test (expected = IllegalArgumentException.class)
    public void unsupportedBukkitColorTest() {
        ANSIColor.getByBukkitColor(ChatColor.MAGIC);
    }
}
