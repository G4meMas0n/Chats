package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.Chats;
import org.bukkit.command.PluginCommand;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.List;

/**
 * Abstract representation of a commands TabExecutor, extends {@link ChatsCommand}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * changed: February 7th, 2020
 */
public abstract class ChatsPluginCommand extends ChatsCommand {

    private PluginCommand command;

    protected ChatsPluginCommand(@NotNull final String name,
                                 @NotNull final String permission,
                                 final int minArgs,
                                 final int maxArgs,
                                 @Nullable final String... aliases) throws IllegalArgumentException {
        super(name, permission, minArgs, maxArgs, aliases);
    }

    @Override
    public final void register(@NotNull final Chats instance) {
        if (this.isRegistered()) {
            return;
        }

        this.command = instance.getCommand(this.getName());

        if (this.command == null) {
            instance.getLogger().warning("Failed to register plugin command '" + this.getName() + "'.");
            return;
        }

        this.command.setExecutor(this);
        this.command.setTabCompleter(this);

        super.register(instance);
    }

    @Override
    public final void unregister() {
        if (!this.isRegistered()) {
            return;
        }

        this.command.setExecutor(null);
        this.command.setTabCompleter(null);
        this.command = null;

        super.unregister();
    }

    @Override
    public final boolean isRegistered() {
        return super.isRegistered() && this.command != null;
    }

    @Override
    public final @NotNull List<String> getAliases() {
        if (this.command == null) {
            return super.getAliases();
        }

        return this.command.getAliases();
    }

    @Override
    public final @NotNull String getDescription() {
        if (this.command == null) {
            return super.getDescription();
        }

        return this.command.getDescription();
    }

    @Override
    public final void setDescription(@NotNull final String description) {
        if (this.command != null) {
            this.command.setDescription(description);
        }

        super.setDescription(description);
    }

    @Override
    public final @NotNull String getPermission() {
        if (this.command == null || this.command.getPermission() == null) {
            return super.getPermission();
        }

        return this.command.getPermission();
    }

    public final @NotNull String getPermissionMessage() {
        if (this.command == null || this.command.getPermissionMessage() == null) {
            return super.getPermissionMessage();
        }

        return this.command.getPermissionMessage();
    }

    public final void setPermissionMessage(@NotNull final String message) {
        if (this.command != null) {
            this.command.setPermissionMessage(message);
        }

        super.setPermissionMessage(message);
    }

    public final @NotNull String getUsage() {
        if (this.command == null) {
            return super.getUsage();
        }

        return this.command.getUsage();
    }

    public final void setUsage(@NotNull final String usage) {
        if (this.command != null) {
            this.command.setUsage(usage);
        }

        super.setUsage(usage);
    }
}
