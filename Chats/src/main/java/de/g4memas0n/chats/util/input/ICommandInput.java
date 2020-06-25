package de.g4memas0n.chats.util.input;

import org.bukkit.ChatColor;
import org.jetbrains.annotations.NotNull;

/**
 * Representation of a command input of a command source.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 23th, 2020
 * changed: June 23th, 2020
 */
public interface ICommandInput {

    /**
     * Represents the enable option for the state "on" ("true").
     */
    @NotNull String ENABLE_OFF = "off";

    /**
     * Represents the enable option for the state "off" ("false").
     */
    @NotNull String ENABLE_ON = "on";

    /**
     * Returns the used command alias of the executed/tab-completed command.
     * @return the used alias.
     */
    @NotNull String getAlias();

    /**
     * Returns the passed command arguments of the executed/tab-completed command.
     * @return the passed arguments.
     */
    @NotNull String[] getArguments();

    /**
     * Returns a new command input that starts on the given start index and uses the previous arguments as alias.
     * @param start index where the new command input starts.
     * @return the new converted command input.
     */
    @NotNull ICommandInput getInput(final int start);

    /**
     * Returns the length of the passed arguments.
     * @return the count of passed arguments.
     */
    int getLength();

    /**
     * Gets the requested chat color by the given argument index, throwing an exception when the arguments at the given
     * index is not a parsable chat color.
     * @param index the index of the argument.
     * @return the parsed chat color.
     * @throws InvalidColorException Thrown when the argument at the given index is not a parsable chat color.
     */
    @NotNull ChatColor getChatColor(final int index) throws InvalidColorException;

    /**
     * Gets the requested format that starts by the given argument index, replacing all '&' to '§'.
     * @param start the start index of the argument.
     * @return the format starting from the given index.
     */
    @NotNull String getFormat(final int start);

    /**
     * Gets the requested message that starts by the given argument index.
     * @param start the start index of the argument.
     * @return the message starting from the given index.
     */
    @NotNull String getMessage(final int start);

    /**
     * Gets the requested index from the command input.
     * @param index the index of the argument.
     * @return the passed argument at the given index.
     */
    @NotNull String get(final int index);

    /**
     * Gets the requested boolean by the given index, throwing an exception when the argument at the given index is not
     * a parsable boolean value.
     * @param index the index of the argument.
     * @return the parsed boolean.
     * @throws InvalidBooleanException Thrown when the argument at the given index is not a parsable boolean.
     */
    boolean getBoolean(final int index) throws InvalidBooleanException;

    /**
     * Gets the requested enable state by the given index, throwing an exception when the argument at the given index
     * is not a parsable enable state.
     * @param index the index of the argument.
     * @return the parsed enable state as boolean.
     * @throws InvalidBooleanException Thrown when the argument at the given index is not a parsable enable state.
     */
    boolean getEnable(final int index) throws InvalidBooleanException;

    /**
     * Gets the requested double by the given index, throwing an exception when the argument at the given index is not
     * a parsable double value.
     * @param index the index of the argument.
     * @return the parsed double value.
     * @throws InvalidNumberException Thrown when the argument at the given index is not a parsable double value.
     */
    @SuppressWarnings("unused")
    double getDouble(final int index) throws InvalidNumberException;

    /**
     * Gets the requested float by the given index, throwing an exception when the argument at the given index is not
     * a parsable float value.
     * @param index the index of the argument.
     * @return the parsed float value.
     * @throws InvalidNumberException Thrown when the argument at the given index is not a parsable float value.
     */
    @SuppressWarnings("unused")
    float getFloat(final int index) throws InvalidNumberException;

    /**
     * Gets the requested integer by the given index, throwing an exception when the argument at the given index is not
     * a parsable integer value.
     * @param index the index of the argument.
     * @return the parsed integer.
     * @throws InvalidNumberException Thrown when the argument at the given index is not a parsable integer.
     */
    int getInteger(final int index) throws InvalidNumberException;

    /**
     * Gets the requested unsigned integer by the given index, throwing an exception when the argument at the given
     * index is not a parsable unsigned integer.
     * @param index the index of the argument.
     * @return the parsed unsigned integer.
     * @throws InvalidNumberException Thrown when the argument at the given index is not a parsable unsigned integer.
     */
    @SuppressWarnings("unused")
    int getUnsignedInteger(final int index) throws InvalidNumberException;

    /**
     * Gets the requested long by the given index, throwing an exception when the argument at the given index is not
     * a parsable long.
     * @param index the index of the argument.
     * @return the parsed long.
     * @throws InvalidNumberException Thrown when the argument at the given index is not a parsable long.
     */
    @SuppressWarnings("unused")
    long getLong(final int index) throws InvalidNumberException;

    /**
     * Gets the requested unsigned long by the given index, throwing an exception when the argument at the given index
     * is not a parsable unsigned long.
     * @param index the index of the argument.
     * @return the parsed unsigned long.
     * @throws InvalidNumberException Thrown when the argument at the given index is not parsable unsigned long.
     */
    long getUnsignedLong(final int index) throws InvalidNumberException;
}
