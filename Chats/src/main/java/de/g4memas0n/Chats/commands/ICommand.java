package de.g4memas0n.Chats.commands;

import de.g4memas0n.Chats.utils.Permission;
import org.bukkit.command.Command;
import org.bukkit.command.TabExecutor;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;

@Deprecated
public interface ICommand extends TabExecutor {

    /**
     * Returns the permission for the given command of the plugin description file, if they is defined.
     * Else it will return a self-build permission with the pattern '[pluginName].command.[commandName]'.
     * @param command the command.
     * @param plugin the plugin that has registered the given command.
     * @return the permission of the given command.
     */
    @NotNull
    static String getPermission(@NotNull final Command command, @NotNull final Plugin plugin) {
        return command.getPermission() != null
                ? command.getPermission()
                : plugin.getDescription().getName().concat(".command.").concat(command.getName());
    }

    /**
     * Returns the permission for the given command of the plugin description file, if they is defined.
     * Else it will return a self-build permission with the pattern '[pluginName].command.[commandName]'.
     * @param command the command.
     * @param pluginName the plugin name that has registered the given command.
     * @return the permission of the given command.
     */
    @NotNull
    static String getPermission(@NotNull final Command command, @NotNull final String pluginName) {
        return command.getPermission() != null
                ? command.getPermission()
                : pluginName.concat(".command").concat(command.getName());
    }

    /**
     * Returns the permission for the given command of the plugin description file, if they is defined.
     * Else it will return the given permission.
     * @param command the command.
     * @param permission the alternate permission.
     * @return the permission of the given command.
     */
    @NotNull
    static String getPermission(@NotNull final Command command, @NotNull final Permission permission) {
        return command.getPermission() != null ? command.getPermission() : permission.toString();
    }
}