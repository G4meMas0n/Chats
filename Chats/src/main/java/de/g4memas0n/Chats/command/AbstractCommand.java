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
 * last change: November 13th, 2019
 */
public abstract class AbstractCommand implements TabExecutor {

    /**
     *
     */
    private static Map<String, AbstractCommand> commands = new HashMap<>();

    private final String name;
    private final String permission;
    private final int minArgs;
    private final int maxArgs;

    private PluginCommand command;

    protected IChats instance;

    protected AbstractCommand(@NotNull final String name,
                              @NotNull final String permission,
                              final int minArgs,
                              final int maxArgs) throws IllegalArgumentException {
        if (minArgs < 0) {
            throw new IllegalArgumentException("the count of minimum arguments is not allowed to be negative.");
        }

        this.name = name;
        this.permission = permission;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;
    }

    public final void register(@NotNull final Chats instance) {
        this.instance = instance;
        this.command = instance.getCommand(this.name);

        if (this.command != null) {
            this.command.setExecutor(this);
            this.command.setTabCompleter(this);

            AbstractCommand.commands.put(this.name, this);

            if (this.instance.getConfigManager().isLogDebug()) {
                this.instance.getLogger().info("Successfully registered Command '" + this.name + "'.");
            }
        } else {
            this.instance.getLogger().warning("Failed to register Command '" + this.name + "'.");
        }
    }

    public final void unregister() {
        this.command.setExecutor(null);
        this.command.setTabCompleter(null);
        this.command = null;

        AbstractCommand.commands.remove(this.name);

        if (this.instance.getConfigManager().isLogDebug()) {
            this.instance.getLogger().info("Successfully unregistered Command '" + this.name + "'.");
        }

        this.instance = null;
    }

    protected final @Nullable AbstractCommand getCommand(@NotNull final String name) {
        if (AbstractCommand.commands.containsKey(name)) {
            return AbstractCommand.commands.get(name);
        }

        return null;
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

    protected final boolean areArgsInRange(final int countArgs) {
        return this.maxArgs > 0 ? countArgs >= this.minArgs && countArgs <= this.maxArgs : countArgs >= this.minArgs;
    }

    /*
    public static boolean updatePermissionMessage(@NotNull final String permissionMessage) {
        boolean successfully = true;
        int count = 0;

        for (AbstractCommand current : AbstractCommand.commands.values()) {
            count++;

            if (current.command != null) {
                current.command.setPermissionMessage(permissionMessage);
            } else {
                successfully = false;
            }
        }

        return count > 0 && successfully;
    }
     */
}
