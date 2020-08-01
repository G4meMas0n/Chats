package de.g4memas0n.chats.command;

import de.g4memas0n.chats.Chats;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.TabExecutor;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * Abstract Plugin Command Representation that represent commands that are registered to bukkit/spigot.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class BasicPluginCommand extends BasicCommand implements TabExecutor {

    private PluginCommand command;

    protected BasicPluginCommand(@NotNull final String name,
                                 final int minArgs,
                                 final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public boolean register(@NotNull final Chats instance) {
        if (this.command != null) {
            return false;
        }

        this.command = instance.getCommand(this.getName());

        if (this.command == null) {
            instance.getLogger().warning("Failed to register command " + this.getName()
                    + "! Is it registered to bukkit/spigot?");
            return false;
        }

        if (super.register(instance)) {
            this.command.setExecutor(this);
            this.command.setTabCompleter(this);
            this.command.setPermissionMessage(tl("noPermission"));
            return true;
        }

        this.command = null;
        return false;
    }

    @Override
    public boolean unregister() {
        if (this.command == null) {
            return false;
        }

        if (super.unregister()) {
            this.command = null;
            return true;
        }

        return false;
    }

    public final @NotNull PluginCommand getCommand() {
        if (this.command == null) {
            throw new IllegalStateException(String.format("Unregistered command '%s' tried to get the plugin command",
                    this.getName()));
        }

        return this.command;
    }

    @Override
    public final @NotNull List<String> getAliases() {
        if (this.command != null) {
            return this.command.getAliases();
        }

        return super.getAliases();
    }

    @Override
    public final void setAliases(@NotNull final List<String> aliases) {
        if (this.command != null) {
            this.command.setAliases(aliases);
        }

        super.setAliases(aliases);
    }

    @Override
    public final @NotNull String getPermission() {
        if (this.command != null && this.command.getPermission() != null) {
            return this.command.getPermission();
        }

        return super.getPermission();
    }

    @Override
    public final void setPermission(@NotNull final String permission) {
        if (this.command != null) {
            this.command.setPermission(permission);
        }

        super.setPermission(permission);
    }

    @Override
    public final boolean onCommand(@NotNull final CommandSender sender,
                                   @NotNull final Command command,
                                   @NotNull final String alias,
                                   @NotNull final String[] arguments) {
        if (this.command == null) {
            this.getInstance().getLogger().severe(String.format("Unregistered plugin command '%s' was executed.", this.getName()));
            return true;
        }

        if (sender.hasPermission(this.getPermission())) {
            final ICommandSource source = sender instanceof Player
                    ? this.getInstance().getChatterManager().getChatter((Player) sender) : new BasicCommandSource(sender);

            try {
                if (this.execute(source, new BasicCommandInput(arguments))) {
                    return true;
                }

                // Invalid command usage. Send syntax help:
                source.sendMessage(this.getDescription());
                source.sendMessage(this.getUsage());
            } catch (InvalidArgumentException ex) {
                source.sendMessage(tl("prefixError") + " " + tl(ex.getKey(), ex.getArguments()));
            } catch (InputException ex) {
                this.getInstance().getLogger().log(Level.SEVERE, String.format("Command execution of '%s' has thrown "
                        + "an unexpected input exception: ", this.getName()), ex);
            }

            return true;
        }

        sender.sendMessage(tl("noPermission"));
        return true;
    }

    @Override
    public final @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                                     @NotNull final Command command,
                                                     @NotNull final String alias,
                                                     @NotNull final String[] arguments) {
        if (this.command == null) {
            this.getInstance().getLogger().severe(String.format("Unregistered plugin command '%s' was tab completed", this.getName()));

            return Collections.emptyList();
        }

        if (sender.hasPermission(this.getPermission())) {
            final ICommandSource source = sender instanceof Player
                    ? this.getInstance().getChatterManager().getChatter((Player) sender) : new BasicCommandSource(sender);

            return this.tabComplete(source, new BasicCommandInput(arguments));
        }

        return Collections.emptyList();
    }
}
