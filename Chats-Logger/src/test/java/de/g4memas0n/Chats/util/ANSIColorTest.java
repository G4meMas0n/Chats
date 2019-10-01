package de.g4memas0n.Chats.util;

import org.bukkit.ChatColor;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Tests the complete ANSIColor Enum.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 6th, 2019
 * last change: August 6th, 2019
 */
public final class ANSIColorTest {

    @Test
    public void getCodeTest() {
        assertEquals("[30m", ANSIColor.BLACK.getCode());
        assertEquals("[34m", ANSIColor.BLUE.getCode());
        assertEquals("[32m", ANSIColor.GREEN.getCode());
        assertEquals("[36m", ANSIColor.CYAN.getCode());
        assertEquals("[31m", ANSIColor.RED.getCode());
        assertEquals("[35m", ANSIColor.MAGENTA.getCode());
        assertEquals("[33m", ANSIColor.YELLOW.getCode());
        assertEquals("[37m", ANSIColor.WHITE.getCode());
        assertEquals("[30;1m", ANSIColor.BRIGHT_BLACK.getCode());
        assertEquals("[34;1m", ANSIColor.BRIGHT_BLUE.getCode());
        assertEquals("[32;1m", ANSIColor.BRIGHT_GREEN.getCode());
        assertEquals("[36;1m", ANSIColor.BRIGHT_CYAN.getCode());
        assertEquals("[31;1m", ANSIColor.BRIGHT_RED.getCode());
        assertEquals("[35;1m", ANSIColor.BRIGHT_MAGENTA.getCode());
        assertEquals("[33;1m", ANSIColor.BRIGHT_YELLOW.getCode());
        assertEquals("[37;1m", ANSIColor.BRIGHT_WHITE.getCode());
        assertEquals("[1m", ANSIColor.BOLD.getCode());
        assertEquals("[4m", ANSIColor.UNDERLINE.getCode());
        assertEquals("[0m", ANSIColor.RESET.getCode());
    }

    @Test
    public void toStringTest() {
        assertEquals("\u001b[30m", ANSIColor.BLACK.toString());
        assertEquals("\u001b[34m", ANSIColor.BLUE.toString());
        assertEquals("\u001b[32m", ANSIColor.GREEN.toString());
        assertEquals("\u001b[36m", ANSIColor.CYAN.toString());
        assertEquals("\u001b[31m", ANSIColor.RED.toString());
        assertEquals("\u001b[35m", ANSIColor.MAGENTA.toString());
        assertEquals("\u001b[33m", ANSIColor.YELLOW.toString());
        assertEquals("\u001b[37m", ANSIColor.WHITE.toString());
        assertEquals("\u001b[30;1m", ANSIColor.BRIGHT_BLACK.toString());
        assertEquals("\u001b[34;1m", ANSIColor.BRIGHT_BLUE.toString());
        assertEquals("\u001b[32;1m", ANSIColor.BRIGHT_GREEN.toString());
        assertEquals("\u001b[36;1m", ANSIColor.BRIGHT_CYAN.toString());
        assertEquals("\u001b[31;1m", ANSIColor.BRIGHT_RED.toString());
        assertEquals("\u001b[35;1m", ANSIColor.BRIGHT_MAGENTA.toString());
        assertEquals("\u001b[33;1m", ANSIColor.BRIGHT_YELLOW.toString());
        assertEquals("\u001b[37;1m", ANSIColor.BRIGHT_WHITE.toString());
        assertEquals("\u001b[1m", ANSIColor.BOLD.toString());
        assertEquals("\u001b[4m", ANSIColor.UNDERLINE.toString());
        assertEquals("\u001b[0m", ANSIColor.RESET.toString());
    }

    @Test
    public void translateBukkitColorTest() {
        final String expected = ANSIColor.RED + "Hello" + ANSIColor.GREEN + "World" + ANSIColor.RESET;
        final String input = ChatColor.DARK_RED + "Hello" + ChatColor.DARK_GREEN + "World";

        assertEquals(expected, ANSIColor.translateBukkitColor(input));
    }

    @Test
    public void translateUnsupportedBukkitColorTest() {
        final String expected = ANSIColor.RED + "HelloWorld" + ANSIColor.RESET;
        final String input = ChatColor.DARK_RED + "Hello" + ChatColor.MAGIC + "World";

        assertEquals(expected, ANSIColor.translateBukkitColor(input));
    }

    @Test
    public void stripEightColorTest() {
        final String expected = "HelloWorld";
        final String input = ANSIColor.RED + "Hello" + ANSIColor.GREEN + "World" + ANSIColor.RESET;

        assertEquals(expected, ANSIColor.stripColor(input));
    }

    @Test
    public void stripSixteenColorTest() {
        final String expected = "HelloWorld";
        final String input = ANSIColor.BRIGHT_RED + "Hello" + ANSIColor.BRIGHT_GREEN + "World" + ANSIColor.RESET;

        assertEquals(expected, ANSIColor.stripColor(input));
    }

    @Test
    public void stripWithoutColorTest() {
        final String expected = "HelloWorld";

        assertEquals(expected, ANSIColor.stripColor(expected));
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
