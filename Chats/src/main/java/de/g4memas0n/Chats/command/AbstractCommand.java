package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.TabExecutor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * Abstract representation of a command, implements the {@link TabExecutor} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: November 19th, 2019
 */
public abstract class AbstractCommand implements TabExecutor {

    private static final String REGISTER_FAILURE = "Failed to register TabExecutor for Command '%s': %s.";
    private static final String REGISTER_SUCCESS = "TabExecutor successfully registered for Command '%s'.";
    private static final String UNREGISTER_FAILURE = "Failed to unregister TabExecutor for Command '%s': %s.";
    private static final String UNREGISTER_SUCCESS = "TabExecutor successfully unregistered for Command '%s'.";

    /**
     * Map of all registered TabExecutors with their name. Used to update all these and to get an another command.
     */
    private static Map<String, AbstractCommand> commands = new HashMap<>();

    private final String name;
    private final String permission;
    private final int minArgs;
    private final int maxArgs;

    private PluginCommand command;
    private IChats instance;

    AbstractCommand(@NotNull final String name,
                    @NotNull final String permission,
                    final int minArgs,
                    final int maxArgs) throws IllegalArgumentException {
        if (name.isEmpty()) {
            throw new IllegalArgumentException("Argument 'name' can not be empty.");
        }

        if (AbstractCommand.commands.containsKey(name)) {
            throw new IllegalArgumentException("Argument 'name' already registered for an another TabExecutor.");
        }

        if (minArgs < 0) {
            throw new IllegalArgumentException("Argument 'minArgs' can not be negative.");
        }

        this.name = name;
        this.permission = permission;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;
    }

    public final void register(@NotNull final Chats instance) {
        final boolean debug = instance.getConfigManager().isLogDebug();

        if (this.instance != null && this.command != null) {
            this.instance.getLogger().warning(String.format(REGISTER_FAILURE, this.name, "TabExecutor already registered"));
            return;
        }

        this.instance = instance;
        this.command = instance.getCommand(this.name);

        if (this.command == null) {
            this.instance.getLogger().warning(String.format(REGISTER_FAILURE, this.name, "Command not registered for this plugin"));
            return;
        }

        this.command.setExecutor(this);
        this.command.setTabCompleter(this);
        this.updateCommand();

        AbstractCommand.commands.put(this.name, this);

        if (debug) {
            this.instance.getLogger().info(String.format(REGISTER_SUCCESS, this.name));
        }
    }

    public final void unregister() {
        if (this.instance == null || this.command == null) {
            Chats.getPluginLogger().warning(String.format(UNREGISTER_FAILURE, this.name, "TabExecutor already unregistered"));
        }

        final boolean debug = this.instance.getConfigManager().isLogDebug();

        this.command.setExecutor(null);
        this.command.setTabCompleter(null);
        this.command = null;

        AbstractCommand.commands.remove(this.name);

        if (debug) {
            this.instance.getLogger().info(String.format(UNREGISTER_SUCCESS, this.name));
        }

        this.instance = null;
    }

    /**
     * Returns the instance of the plugins main class.
     * As long as the command is registered, this method will not return null.
     * @return the plugin main class instance.
     */
    protected final @NotNull IChats getInstance() {
        return this.instance;
    }

    /**
     * Returns the Command TabExecutor implementation of the given command name.
     * @param name the name of the command.
     * @return the Command TabExecutor when it is registered, null otherwise.
     */
    protected final @Nullable AbstractCommand getExecutor(@NotNull final String name) {
        return AbstractCommand.commands.get(name);
    }

    protected final @NotNull String getName() {
        return this.name;
    }

    protected final @NotNull String getPermission() {
        if (this.command == null) {
            return this.permission;
        }

        return this.command.getPermission() != null ? this.command.getPermission() : this.permission;
    }

    protected final int getMinParam() {
        return this.minArgs;
    }

    protected final int getMaxParam() {
        return this.maxArgs;
    }

    protected final boolean isArgsInRange(final int countArgs) {
        return this.maxArgs > 0 ? countArgs >= this.minArgs && countArgs <= this.maxArgs : countArgs >= this.minArgs;
    }

    private void updateCommand() {
        if (this.command == null) {
            return;
        }

        //TODO: Insert localized Command Description, Usage and Permission Message.
        this.command.setDescription("");
        this.command.setUsage("");
        this.command.setPermissionMessage("");
    }

    public static void updateCommands() {
        for (AbstractCommand current : AbstractCommand.commands.values()) {
            current.updateCommand();
        }
    }
}
