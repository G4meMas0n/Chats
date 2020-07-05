package de.g4memas0n.chats.command;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.CommandSource;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ChannelNotExistException;
import de.g4memas0n.chats.util.input.CommandInput;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidArgumentException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import de.g4memas0n.chats.util.logging.Log;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.TabExecutor;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

/**
 * Abstract Plugin Command Representation that represent commands that are registered to bukkit/spigot.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 25th, 2020
 * changed: July 5th, 2020
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
    public final @NotNull String getDescription() {
        if (this.command != null && !this.command.getDescription().isEmpty()) {
            return this.command.getDescription();
        }

        return super.getDescription();
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
    public final @NotNull String getUsage() {
        if (this.command != null && !this.command.getUsage().isEmpty()) {
            return this.command.getUsage();
        }

        return super.getUsage();
    }

    @Override
    public final void setUsage(@NotNull final String usage) {
        if (this.command != null) {
            this.command.setUsage(usage);
        }

        super.setUsage(usage);
    }

    @Override
    public final boolean onCommand(@NotNull final CommandSender sender,
                                   @NotNull final Command command,
                                   @NotNull final String alias,
                                   @NotNull final String[] arguments) {
        if (this.command == null) {
            Log.getPlugin().severe(String.format("Unregistered plugin command '%s' was executed.", this.getName()));
            return true;
        }

        if (sender.hasPermission(this.getPermission())) {
            final ICommandSource source = sender instanceof Player
                    ? this.getInstance().getChatterManager().getChatter((Player) sender)
                    : new CommandSource(sender);
            final ICommandInput input = new CommandInput(arguments);

            try {
                if (this.execute(source, input)) {
                    return true;
                }

                for (final String message : this.help(source, input)) {
                    source.sendMessage(message);
                }
            } catch (ChannelNotExistException ex) {
                source.sendMessage(Messages.tlErr("channelNotExist", ex.getChannel()));
            } catch (PlayerNotFoundException ex) {
                source.sendMessage(Messages.tlErr("playerNotFound", ex.getPlayer()));
            } catch (InvalidArgumentException ex) {
                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getArguments()));
            } catch (InputException ex) {
                Log.getPlugin().log(Level.SEVERE, String.format("Command execution of '%s' has thrown an unexpected input exception: ", this.getName()), ex);
            }

            return true;
        }

        sender.sendMessage(Messages.tl("noPermission"));
        return true;
    }

    @Override
    public final @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                                     @NotNull final Command command,
                                                     @NotNull final String alias,
                                                     @NotNull final String[] arguments) {
        if (this.command == null) {
            Log.getPlugin().severe(String.format("Unregistered plugin command '%s' was tab completed", this.getName()));
            return Collections.emptyList();
        }

        if (sender.hasPermission(this.getPermission())) {
            final ICommandSource source = sender instanceof Player
                    ? this.getInstance().getChatterManager().getChatter((Player) sender) : new CommandSource(sender);

            return this.tabComplete(source, new CommandInput(arguments));
        }

        return Collections.emptyList();
    }
}
