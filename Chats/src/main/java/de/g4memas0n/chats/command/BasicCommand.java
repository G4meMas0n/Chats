package de.g4memas0n.chats.command;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.info.HelpCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Abstract Command Representation that represents all non bukkit/spigot commands.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 24th, 2020
 * changed: July 5th, 2020
 */
public abstract class BasicCommand {

    /**
     * Collection of all {@link BasicCommand} that are registered by using the {@link BasicCommand#register(Chats)}
     * method.
     *
     * This allows all registered commands to access other registered commands, such as execute or tab complete an
     * other registered command. So access to this Map will only occur through the{@link BasicCommand#getRegistered()}
     * and {@link BasicCommand#getRegistered(String)} methods that are only available for command classes.
     *
     * This is handled in this way, because currently this is only used in {@link HelpCommand} to access the
     * information of other commands for displaying their command help. Also for executing or tab-completing commands
     * the bukkit/spigot command system is used (See {@link BasicPluginCommand}). So using an extra command handler
     * would be useless, because it would only be a collection of commands (like this).
     */
    private static final Map<String, BasicCommand> registered = new LinkedHashMap<>();

    private final String name;
    private final int minArgs;
    private final int maxArgs;

    private IChats instance;

    private List<String> aliases;
    private String description;
    private String permission;
    private String usage;

    protected BasicCommand(@NotNull final String name,
                           final int minArgs,
                           final int maxArgs) {
        this.name = name;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;

        this.description = "";
        this.permission = "";
        this.usage = "";
    }

    public boolean register(@NotNull final Chats instance) {
        if (registered.containsKey(this.name)) {
            return false;
        }

        this.instance = instance;

        registered.put(this.name, this);
        return true;
    }

    public boolean unregister() {
        if (!registered.containsKey(this.name)) {
            return false;
        }

        this.instance = null;

        registered.remove(this.name, this);
        return true;
    }

    public final @NotNull IChats getInstance() {
        if (this.instance == null || !registered.containsKey(this.name)) {
            throw new IllegalStateException(String.format("Unregistered command '%s' tried to get the plugin instance",
                    this.getName()));
        }

        return this.instance;
    }

    public final @NotNull String getName() {
        return this.name;
    }

    public final int getMinArgs() {
        return this.minArgs;
    }

    public final int getMaxArgs() {
        return this.maxArgs;
    }

    public final boolean argsInRange(final int arguments) {
        return this.maxArgs > 0
                ? arguments >= this.minArgs && arguments <= this.maxArgs
                : arguments >= this.minArgs;
    }

    /**
     * Executes the command for the given sender, returning its success.
     * If false is returned, then the help of the command will be sent to the sender.
     * @param sender the source who executed the command.
     * @param input the input of the sender, including used alias and passed arguments.
     * @return true if the command execution was valid, false otherwise.
     */
    public abstract boolean execute(@NotNull final ICommandSource sender,
                                    @NotNull final ICommandInput input) throws InputException;

    public @NotNull List<String> help(@NotNull final ICommandSource sender,
                                      @NotNull final ICommandInput input) {
        final List<String> help = new ArrayList<>();

        help.add(Messages.tl("helpHeader", this.name));
        help.add(Messages.tl("helpDescription", this.description));
        help.add(Messages.tl("helpUsage", this.usage));

        if (this.aliases != null && !this.aliases.isEmpty()) {
            help.add(Messages.tlJoin("helpAliases", this.aliases));
        }

        return help;
    }

    /**
     * Requests a list of possible completions for a command argument.
     * @param sender the source who tab-completed the command.
     * @param input the input of the sender, including used alias and the passed arguments including the final partial
     *              argument to be completed.
     * @return a list of possible completions for the final arguments.
     */
    public abstract @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                      @NotNull final ICommandInput input);

    public @NotNull List<String> getAliases() {
        if (this.aliases == null) {
            return Collections.emptyList();
        }

        return this.aliases;
    }

    public void setAliases(@NotNull final List<String> aliases) {
        if (aliases.equals(this.aliases)) {
            return;
        }

        this.aliases = Collections.unmodifiableList(aliases);
    }

    public @NotNull String getDescription() {
        return this.description;
    }

    public void setDescription(@NotNull final String description) {
        if (description.equals(this.description)) {
            return;
        }

        this.description = description;
    }

    public @NotNull String getPermission() {
        return this.permission;
    }

    public void setPermission(@NotNull final String permission) {
        if (permission.equals(this.permission)) {
            return;
        }

        this.permission = permission;
    }

    public @NotNull String getUsage() {
        return this.usage;
    }

    public void setUsage(@NotNull final String usage) {
        if (usage.equals(this.usage)) {
            return;
        }

        this.usage = usage;
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name=");
        builder.append(this.getName());
        builder.append(";min-args=");
        builder.append(this.getMinArgs());
        builder.append(";max-args=");
        builder.append(this.getMaxArgs());

        if (!this.getAliases().isEmpty()) {
            builder.append(";aliases=");
            builder.append(String.join(",", this.getAliases()));
        }

        if (!this.getDescription().isEmpty()) {
            builder.append(";description=");
            builder.append(this.getDescription());
        }

        if (!this.getPermission().isEmpty()) {
            builder.append(";permission=");
            builder.append(this.getPermission());
        }

        if (!this.getUsage().isEmpty()) {
            builder.append(";usage=");
            builder.append(this.getUsage());
        }

        return builder.append("}").toString();
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (object == this) {
            return true;
        }

        if (object instanceof BasicCommand) {
            final BasicCommand other = (BasicCommand) object;

            return this.getName().equals(other.getName())
                    && this.getMinArgs() == other.getMinArgs()
                    && this.getMaxArgs() == other.getMaxArgs();
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 69;
        int result = 2;

        result = prime * result + this.getName().hashCode();
        result = prime * result + Integer.hashCode(this.getMinArgs());
        result = prime * result + Integer.hashCode(this.getMaxArgs());

        return result;
    }

    // Methods for accessing registered Commands:
    protected final @NotNull Set<BasicCommand> getRegistered() {
        return new LinkedHashSet<>(registered.values());
    }

    protected final @Nullable BasicCommand getRegistered(@NotNull final String name) {
        final BasicCommand command = registered.get(name.toLowerCase());

        if (command != null) {
            return command;
        }

        for (final BasicCommand current : registered.values()) {
            for (final String alias : current.getAliases()) {
                if (alias.equalsIgnoreCase(name)) {
                    return current;
                }
            }
        }

        return null;
    }
}
