package de.g4memas0n.chats.util.input;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;
import java.util.Arrays;

/**
 * Implementation of a users command input.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public class CommandInput implements ICommandInput {

    private final String alias;
    private final String[] arguments;

    public CommandInput(@NotNull final String alias,
                        @NotNull final String[] arguments) {
        this.alias = alias;
        this.arguments = arguments;
    }

    @Override
    public @NotNull String getAlias() {
        return this.alias;
    }

    @Override
    public @NotNull String[] getArguments() {
        return this.arguments;
    }

    @Override
    public @NotNull ICommandInput getInput(final int start) {
        final String[] arguments = this.arguments.length == start ? new String[0]
                : Arrays.copyOfRange(this.arguments, start, this.arguments.length);

        return new CommandInput(this.arguments[start - 1], arguments);
    }

    @Override
    public int getLength() {
        return this.arguments.length;
    }

    @Override
    public @NotNull ChatColor getChatColor(final int index) throws InvalidColorException {
        try {
            return ChatColor.valueOf(this.arguments[index].toUpperCase());
        } catch (IllegalArgumentException ex) {
            throw new InvalidColorException(ex);
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
    public @NotNull String get(final int index) {
        return this.arguments[index];
    }

    @Override
    public boolean getBoolean(final int index) throws InvalidBooleanException {
        if (this.arguments[index].equalsIgnoreCase(Boolean.FALSE.toString())) {
            return false;
        } else if (this.arguments[index].equalsIgnoreCase(Boolean.TRUE.toString())) {
            return true;
        } else {
            throw new InvalidBooleanException();
        }
    }

    @Override
    public boolean getEnable(final int index) throws InvalidBooleanException {
        if (this.arguments[index].equalsIgnoreCase(ENABLE_OFF)) {
            return false;
        } else if (this.arguments[index].equalsIgnoreCase(ENABLE_ON)) {
            return true;
        } else {
            throw new InvalidBooleanException("invalidState");
        }
    }

    @Override
    public double getDouble(final int index) throws InvalidNumberException {
        try {
            final double result = Double.parseDouble(this.arguments[index]);

            if (Double.isNaN(result) || Double.isInfinite(result)) {
                throw new InvalidNumberException();
            }

            return result;
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }

    @Override
    public float getFloat(final int index) throws InvalidNumberException {
        try {
            final float result = Float.parseFloat(this.arguments[index]);

            if (Float.isNaN(result) || Float.isInfinite(result)) {
                throw new InvalidNumberException();
            }

            return result;
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }

    @Override
    public int getInteger(final int index) throws InvalidNumberException {
        try {
            return Integer.parseInt(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }

    @Override
    public int getUnsignedInteger(final int index) throws InvalidNumberException {
        try {
            return Integer.parseUnsignedInt(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }

    @Override
    public long getLong(final int index) throws InvalidNumberException {
        try {
            return Long.parseLong(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }

    @Override
    public long getUnsignedLong(final int index) throws InvalidNumberException {
        try {
            return Long.parseUnsignedLong(this.arguments[index]);
        } catch (NumberFormatException ex) {
            throw new InvalidNumberException(ex);
        }
    }
}
