package de.g4memas0n.chats.command;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.util.Arrays;

/**
 * Implementation of a users command input.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class BasicCommandInput implements ICommandInput {

    private final String[] arguments;

    public BasicCommandInput() {
        this.arguments = new String[0];
    }

    public BasicCommandInput(@NotNull final String[] arguments) {
        this.arguments = arguments;
    }

    @Override
    public @NotNull String[] getArguments() {
        return this.arguments;
    }

    @Override
    public @NotNull ICommandInput getInput(final int start) {
        if (this.arguments.length <= start) {
            return new BasicCommandInput();
        }

        return new BasicCommandInput(Arrays.copyOfRange(this.arguments, start, this.arguments.length));
    }

    @Override
    public int getLength() {
        return this.arguments.length;
    }

    @Override
    public @NotNull String get(final int index) {
        return this.arguments[index];
    }

    @Override
    public @NotNull ChatColor getChatColor(final int index) throws InvalidArgumentException {
        try {
            return ChatColor.valueOf(this.arguments[index].toUpperCase());
        } catch (IllegalArgumentException ex) {
            throw new InvalidArgumentException("invalidColor", this.arguments[index]);
        }
    }

    @Override
    public @NotNull String getFormat(final int start) {
        final StringBuilder format = new StringBuilder();

        for (int index = start; index < this.arguments.length; index++) {
            format.append(this.arguments[index]).append(" ");
        }

        return ChatColor.translateAlternateColorCodes('&', format.toString().trim());
    }

    @Override
    public @NotNull String getMessage(final int start) {
        final StringBuilder message = new StringBuilder();

        for (int index = start; index < this.arguments.length; index++) {
            message.append(this.arguments[index]).append(" ");
        }

        return message.toString().trim();
    }

    @Override
    public boolean getBoolean(final int index) throws InvalidArgumentException {
        final String bool = this.arguments[index];

        if (bool.equalsIgnoreCase("false")) {
            return false;
        }

        if (bool.equalsIgnoreCase("true")) {
            return true;
        }

        throw new InvalidArgumentException("invalidBoolean", bool);
    }

    @Override
    public boolean getEnable(final int index) throws InvalidArgumentException {
        final String bool = this.arguments[index];

        if (bool.equalsIgnoreCase("off") || bool.equalsIgnoreCase("enable")) {
            return false;
        }

        if (bool.equalsIgnoreCase("on") || bool.equalsIgnoreCase("disable")) {
            return true;
        }

        throw new InvalidArgumentException("invalidState", bool);
    }

    @Override
    public double getDouble(final int index) throws InvalidArgumentException {
        try {
            final double result = Double.parseDouble(this.arguments[index]);

            if (!Double.isNaN(result) && !Double.isInfinite(result)) {
                return result;
            }
        } catch (NumberFormatException ignored) {

        }

        throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
    }

    @Override
    public float getFloat(final int index) throws InvalidArgumentException {
        try {
            final float result = Float.parseFloat(this.arguments[index]);

            if (!Float.isNaN(result) && !Float.isInfinite(result)) {
                return result;
            }
        } catch (NumberFormatException ignored) {

        }

        throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
    }

    @Override
    public int getInteger(final int index) throws InvalidArgumentException {
        try {
            return Integer.parseInt(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
        }
    }

    @Override
    public int getUnsignedInteger(final int index) throws InvalidArgumentException {
        try {
            return Integer.parseUnsignedInt(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
        }
    }

    @Override
    public long getLong(final int index) throws InvalidArgumentException {
        try {
            return Long.parseLong(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
        }
    }

    @Override
    public long getUnsignedLong(final int index) throws InvalidArgumentException {
        try {
            return Long.parseUnsignedLong(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidArgumentException("invalidNumber", this.arguments[index]);
        }
    }
}
