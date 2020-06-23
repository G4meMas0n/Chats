package de.g4memas0n.chats.command;

import de.g4memas0n.chats.Chats;
import de.g4memas0n.chats.chatter.CommandSource;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.chatter.ChatterCommand;
import de.g4memas0n.chats.command.delegate.DelegateCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.CommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidBooleanException;
import de.g4memas0n.chats.util.input.InvalidChannelException;
import de.g4memas0n.chats.util.input.InvalidColorException;
import de.g4memas0n.chats.util.input.InvalidFormatException;
import de.g4memas0n.chats.util.input.InvalidNameException;
import de.g4memas0n.chats.util.input.InvalidNumberException;
import de.g4memas0n.chats.util.input.InvalidPasswordException;
import de.g4memas0n.chats.util.input.InvalidPlayerException;
import de.g4memas0n.chats.util.input.InvalidTypeException;
import de.g4memas0n.chats.util.logging.Log;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.TabExecutor;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
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
 * changed: June 23th, 2020
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
                                   @NotNull final Command ignored,
                                   @NotNull final String alias,
                                   @NotNull final String[] arguments) {
        if (this.command == null) {
            Log.getPlugin().severe(String.format("Unregistered plugin command '%s' was executed.", this.getName()));
            return true;
        }

        if (sender.hasPermission(this.getPermission())) {
            final ICommandSource source = sender instanceof Player ? this.getInstance().getChatterManager().getChatter((Player) sender) : new CommandSource(sender);

            try {
                if (!this.execute(source, new CommandInput(alias, arguments))) {
                    sender.sendMessage(Messages.tl("helpHeader", this.getName()));
                    sender.sendMessage(Messages.tl("helpDescription", this.getDescription()));
                    sender.sendMessage(Messages.tl("helpUsage", this.getUsage()));

                    if (!this.getAliases().isEmpty()) {
                        sender.sendMessage(Messages.tlJoin("helpAliases", this.getAliases()));
                    }

                    if (this instanceof DelegateCommand) {
                        final List<String> commands = new ArrayList<>();

                        for (final BasicCommand command : ((DelegateCommand) this).getCommands()) {
                            if (command instanceof ChatterCommand && !(source instanceof IChatter)) {
                                continue;
                            }

                            if (sender.hasPermission(command.getPermission())) {
                                commands.add(command.getName());
                            }
                        }

                        Collections.sort(commands);

                        sender.sendMessage(Messages.tlJoin("helpCommands", commands));
                    }
                }

                return true;
            } catch (InvalidBooleanException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidBoolean"));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey()));
            } catch (InvalidChannelException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("channelNotExist", ex.getName()));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getName()));
            } catch (InvalidColorException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidColor"));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey()));
            } catch (InvalidFormatException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidFormat", Messages.tl(ex.getFormat())));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), Messages.tl(ex.getFormat())));
            } catch (InvalidNameException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidName", ex.getName()));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getName()));
            } catch (InvalidNumberException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidNumber"));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey()));
            } catch (InvalidPasswordException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidPassword", ex.getPassword()));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getPassword()));
            } catch (InvalidPlayerException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("playerNotFound", ex.getName()));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getName()));
            } catch (InvalidTypeException ex) {
                if (ex.getKey() == null) {
                    source.sendMessage(Messages.tlErr("invalidType", ex.getIdentifier()));
                    return true;
                }

                source.sendMessage(Messages.tlErr(ex.getKey(), ex.getIdentifier()));
            } catch (InputException ex) {
                Log.getPlugin().log(Level.SEVERE, "Command execution has thrown an unexpected exception: ", ex);
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

            return this.tabComplete(source, new CommandInput(alias, arguments));
        }

        return Collections.emptyList();
    }
}
