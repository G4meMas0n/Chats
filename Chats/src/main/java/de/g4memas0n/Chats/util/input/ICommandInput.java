package de.g4memas0n.chats.util.input;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;

public interface ICommandInput {

    @NotNull String ENABLE_OFF = "off";
    @NotNull String ENABLE_ON = "on";

    @NotNull String getAlias();

    @NotNull String[] getArguments();

    @NotNull ICommandInput getInput(final int start);

    int getLength();

    @NotNull ChatColor getChatColor(final int index) throws InvalidColorException;

    @NotNull String getFormat(final int start);

    @NotNull String getMessage(final int start);

    @NotNull String get(final int index);

    boolean getBoolean(final int index) throws InvalidBooleanException;

    boolean getEnable(final int index) throws InvalidBooleanException;

    double getDouble(final int index) throws InvalidNumberException;

    float getFloat(final int index) throws InvalidNumberException;

    int getInteger(final int index) throws InvalidNumberException;

    int getUnsignedInteger(final int index) throws InvalidNumberException;

    long getLong(final int index) throws InvalidNumberException;

    long getUnsignedLong(final int index) throws InvalidNumberException;
}
