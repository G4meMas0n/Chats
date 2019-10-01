package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.IChats;
import org.bukkit.command.Command;
import org.bukkit.command.TabExecutor;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract representation of a command, implements the {@link TabExecutor} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: October 1st, 2019
 */
public abstract class AbstractCommand implements TabExecutor {

    /**
     * the alternate permission for the command. Used when no permission is defined in the plugin.yml.
     */
    protected final String altPermission;

    /**
     * the max parameter count, that is allowed for the command.
     */
    protected final int maxParam;

    protected IChats chatsInstance;

    protected AbstractCommand(@NotNull final String permission,
                              final int maxParam) {
        this.altPermission = permission;
        this.maxParam = maxParam;
    }

    public final void setInstance(@NotNull final IChats instance) {
        this.chatsInstance = instance;
    }

    protected final @NotNull String getPermission(@NotNull final Command command) {
        return command.getPermission() != null ? command.getPermission() : this.altPermission;
    }

    protected final int getMaxParam() {
        return maxParam;
    }

    public static @NotNull String getPermission(@NotNull final Command command, @NotNull final String altPermission) {
        return command.getPermission() != null ? command.getPermission() : altPermission;
    }
}
