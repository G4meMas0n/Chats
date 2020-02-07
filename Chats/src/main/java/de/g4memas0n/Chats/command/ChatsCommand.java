package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.util.ConfigKey;
import org.bukkit.command.TabExecutor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Abstract representation of a SubCommand, implements {@link TabExecutor}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 12th, 2020
 * changed: January 31th, 2020
 */
public abstract class ChatsCommand implements TabExecutor {

    private static final Map<String, ChatsCommand> registered = new HashMap<>();

    private final List<String> aliases;
    private final String name;
    private final String permission;
    private final int minArgs;
    private final int maxArgs;

    private String descriptionMessage;
    private String permissionMessage;
    private String usageMessage;

    private IChats instance;

    public ChatsCommand(@NotNull final String name,
                        @NotNull final String permission,
                        final int minArgs,
                        final int maxArgs,
                        @Nullable final String... aliases) throws IllegalArgumentException {
        if (minArgs < 0) {
            throw new IllegalArgumentException("Invalid number! Argument 'minArgs' can not be negative");
        }

        if (maxArgs >= 0 && maxArgs < minArgs) {
            throw new IllegalArgumentException("Invalid number! Argument 'maxArgs' must be negative or "
                    + "bigger than 'minArgs'");
        }

        this.name = name;
        this.permission = permission;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;
        this.aliases = new ArrayList<>();

        if (aliases != null) {
            this.aliases.addAll(Arrays.asList(aliases));
        }
    }

    public void register(@NotNull final Chats instance) {
        if (this.isRegistered()) {
            return;
        }

        this.instance = instance;

        ChatsCommand.registered.put(this.getName(), this);

        if (this.instance.getConfig().getBoolean(ConfigKey.LOG_DEBUG.getPath())) {
            this.instance.getLogger().info("CommandHandler: Registered TabExecutor " + this);
        }
    }

    public void unregister() {
        if (!this.isRegistered()) {
            return;
        }

        ChatsCommand.registered.remove(this.getName());

        if (this.instance.getConfig().getBoolean(ConfigKey.LOG_DEBUG.getPath())) {
            this.instance.getLogger().info("CommandHandler: Unregistered TabExecutor " + this);
        }

        this.instance = null;
    }

    public boolean isRegistered() {
        return this.instance != null;
    }

    public @NotNull List<String> getAliases() {
        return this.aliases;
    }

    public @NotNull String getDescription() {
        return this.descriptionMessage;
    }

    public void setDescription(@NotNull final String description) {
        if (description.equals(this.descriptionMessage)) {
            return;
        }

        this.descriptionMessage = description;
    }

    public @NotNull String getPermission() {
        return this.permission;
    }

    public @NotNull String getPermissionMessage() {
        return this.permissionMessage;
    }

    public void setPermissionMessage(@NotNull final String permissionMessage) {
        if (permissionMessage.equals(this.permissionMessage)) {
            return;
        }

        this.permissionMessage = permissionMessage;
    }

    public @NotNull String getUsage() {
        return this.usageMessage;
    }

    public void setUsage(@NotNull final String usage) {
        if (usage.equals(this.usageMessage)) {
            return;
        }

        this.usageMessage = usage;
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

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{name='");
        builder.append(this.getName());

        if (!this.getAliases().isEmpty()) {
            builder.append("';aliases='");
            builder.append(String.join("','", this.getAliases()));
        }

        builder.append("';description='");
        builder.append(this.getDescription());
        builder.append("';permission='");
        builder.append(this.getPermission());
        builder.append("';usage='");
        builder.append(this.getUsage());
        builder.append("';minArgs=");
        builder.append(this.getMinArgs());
        builder.append("';maxArgs=");
        builder.append(this.getMaxArgs());

        return builder.toString();
    }

    protected final boolean argsInRange(final int countArgs) {
        return this.maxArgs > 0 ? countArgs >= this.minArgs && countArgs <= this.maxArgs : countArgs >= this.minArgs;
    }

    protected final @NotNull IChats getInstance() {
        if (!this.isRegistered() || this.instance == null) {
            throw new IllegalStateException("Illegal Access! Unregistered TabExecutor " + this
                    + " tried to get the plugin instance");
        }

        return this.instance;
    }

    protected final @Nullable ChatsCommand getRegistered(@NotNull final String name) {
        return registered.get(name);
    }

    protected final boolean isRegistered(@NotNull final String name) {
        return registered.containsKey(name);
    }

    protected static @NotNull String getMessage(@NotNull final String[] arguments, final int begin) {
        final StringBuilder builder = new StringBuilder();

        for (int i = begin; i < arguments.length; i++) {
            builder.append(arguments[i]).append(" ");
        }

        return builder.toString().trim();
    }
}
