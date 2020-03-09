package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.messaging.Messages;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.TabExecutor;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

public abstract class BasicPluginCommand extends BasicCommand implements TabExecutor {

    private PluginCommand command;

    protected BasicPluginCommand(@NotNull final String name,
                                 @NotNull final String permission,
                                 final int minArgs,
                                 final int maxArgs) {
        super(name, permission, minArgs, maxArgs);
    }

    protected BasicPluginCommand(@NotNull final String name,
                                 @NotNull final String permission,
                                 final int minArgs,
                                 final int maxArgs,
                                 @NotNull final List<String> aliases) {
        super(name, permission, minArgs, maxArgs, aliases);
    }

    @Override
    public final boolean register(@NotNull final Chats instance) {
        if (this.isRegistered()) {
            return false;
        }

        this.command = instance.getCommand(this.getName());

        if (this.command == null) {
            instance.getLogger().warning("Failed to register command " + this.getName()
                    + "! Is it registered to spigot?");
            return false;
        }

        this.command.setExecutor(this);
        this.command.setTabCompleter(this);

        return super.register(instance);
    }

    @Override
    public final boolean unregister() {
        if (!this.isRegistered()) {
            return false;
        }

        this.command = null;

        return super.unregister();
    }

    @Override
    public final boolean isRegistered() {
        return super.isRegistered() && this.command != null;
    }

    @Override
    public final @NotNull String getPermission() {
        if (this.isRegistered() && this.command.getPermission() != null && !this.command.getPermission().isEmpty()) {
            return this.command.getPermission();
        }

        return super.getPermission();
    }

    @Override
    public final @NotNull List<String> getAliases() {
        if (this.isRegistered()) {
            return this.command.getAliases();
        }

        return super.getAliases();
    }

    @Override
    public final void setAliases(@NotNull final List<String> aliases) {
        if (this.isRegistered()) {
            this.command.setAliases(aliases);
        }

        super.setAliases(aliases);
    }

    @Override
    public final @NotNull String getDescription() {
        if (this.isRegistered()) {
            return this.command.getDescription();
        }

        return super.getDescription();
    }

    @Override
    public final void setDescription(@NotNull final String description) {
        if (this.isRegistered()) {
            this.command.setDescription(description);
        }

        super.setDescription(description);
    }

    @Override
    public final @NotNull String getUsage() {
        if (this.isRegistered()) {
            return this.command.getUsage();
        }

        return super.getUsage();
    }

    @Override
    public final void setUsage(@NotNull final String usage) {
        if (this.isRegistered()) {
            this.command.setUsage(usage);
        }

        super.setUsage(usage);
    }

    @Override
    public final boolean onCommand(@NotNull final CommandSender sender,
                                   @NotNull final Command command,
                                   @NotNull final String alias,
                                   @NotNull final String[] arguments) {
        if (!this.isRegistered()) {
            sender.sendMessage(Messages.tl("notRegistered"));
            return true;
        }

        if (!sender.hasPermission(this.getPermission())) {
            sender.sendMessage(Messages.tl("noPermission"));
            return true;
        }

        if (!this.execute(sender, alias, arguments)) {
            sender.sendMessage(this.getDescription());
            sender.sendMessage(this.getUsage());
        }

        return true;
    }

    @Override
    public final @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                                     @NotNull final Command command,
                                                     @NotNull final String alias,
                                                     @NotNull final String[] arguments) {
        if (!this.isRegistered() || !sender.hasPermission(this.getPermission())) {
            return Collections.emptyList();
        }

        return this.tabComplete(sender, alias, arguments);
    }
}
