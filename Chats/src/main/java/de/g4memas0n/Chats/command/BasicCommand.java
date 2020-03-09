package de.g4memas0n.Chats.command;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.chatter.IPermissible;
import de.g4memas0n.Chats.chatter.SenderPermissible;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class BasicCommand {

    private static final Map<String, BasicCommand> registered = new LinkedHashMap<>();

    private final String name;
    private final String permission;
    private final int minArgs;
    private final int maxArgs;

    private List<String> aliases;
    private String description;
    private String usage;

    private IChats instance;

    protected BasicCommand(@NotNull final String name,
                           @NotNull final String permission,
                           final int minArgs,
                           final int maxArgs) {
        this.name = name;
        this.permission = permission;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;

        this.aliases = new ArrayList<>();
    }

    protected BasicCommand(@NotNull final String name,
                           @NotNull final String permission,
                           final int minArgs,
                           final int maxArgs,
                           @NotNull final List<String> aliases) {
        this.name = name;
        this.permission = permission;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;

        this.aliases = new ArrayList<>(aliases);
    }

    public boolean register(@NotNull final Chats instance) {
        if (this.isRegistered()) {
            return false;
        }

        this.instance = instance;

        registered.put(this.name, this);

        if (instance.getSettings().isLogDebug()) {
            instance.getLogger().info("Registered command " + this);
        }

        return true;
    }

    public boolean unregister() {
        if (!this.isRegistered()) {
            return false;
        }

        registered.remove(this.name);

        if (this.instance.getSettings().isLogDebug()) {
            instance.getLogger().info("Unregistered command " + this);
        }

        this.instance = null;

        return true;
    }

    public boolean isRegistered() {
        return registered.containsKey(this.name) && this.instance != null;
    }

    public final @NotNull String getName() {
        return this.name;
    }

    public @NotNull String getPermission() {
        return this.permission;
    }

    public boolean hasAliases() {
        return !this.aliases.isEmpty();
    }

    public @NotNull List<String> getAliases() {
        return new ArrayList<>(this.aliases);
    }

    public void setAliases(@NotNull final List<String> aliases) {
        if (aliases.equals(this.aliases)) {
            return;
        }

        this.aliases = new ArrayList<>(aliases);
    }

    public @NotNull String getDescription() {
        return this.description;
    }

    public void setDescription(@NotNull final String description) {
        if (description.isEmpty() || description.equals(this.description)) {
            return;
        }

        this.description = description;
    }

    public @NotNull String getUsage() {
        return this.usage;
    }

    public void setUsage(@NotNull final String usage) {
        if (usage.isEmpty() || usage.equals(this.usage)) {
            return;
        }

        this.usage = usage;
    }

    public final int getMinArgs() {
        return this.minArgs;
    }

    public final int getMaxArgs() {
        return this.maxArgs;
    }

    @Override
    public final @NotNull String toString() {
        return this.getClass().getSimpleName()
                + "{name='" + this.getName() + "'"
                + ";permission='" + this.getPermission() + "'"
                + ";minArgs=" + this.getMinArgs()
                + ";maxArgs=" + this.getMaxArgs()
                + ";description='" + this.getDescription() + "'"
                + ";usage='" + this.getUsage() + "'"
                + ";aliases='" + String.join("','", this.getAliases()) + "'"
                + ";registered=" + this.isRegistered() + "}";
    }

    public final boolean argsInRange(final int arguments) {
        return this.maxArgs > 0 ? arguments >= this.minArgs && arguments <= this.maxArgs : arguments >= this.minArgs;
    }

    public abstract boolean execute(@NotNull final CommandSender sender,
                                    @NotNull final String alias,
                                    @NotNull final String[] arguments);

    public abstract @NotNull List<String> tabComplete(@NotNull final CommandSender sender,
                                                      @NotNull final String alias,
                                                      @NotNull final String[] arguments);

    protected final @NotNull IChats getInstance() {
        if (!this.isRegistered()) {
            throw new IllegalStateException("Unregistered Command " + this + " tried to get the plugin instance");
        }

        return this.instance;
    }

    protected final @NotNull IPermissible getPermissible(@NotNull final CommandSender sender) {
        if (sender instanceof Player) {
            return this.getInstance().getChatterManager().getChatter((Player) sender);
        }

        return new SenderPermissible(sender);
    }

    protected final @NotNull Set<BasicCommand> getRegistered() {
        return new LinkedHashSet<>(registered.values());
    }

    protected final @Nullable BasicCommand getRegistered(@NotNull final String name) {
        return registered.get(name);
    }

    protected static @NotNull String[] copyArguments(@NotNull final String[] arguments, final int from) {
        return Arrays.copyOfRange(arguments, from, arguments.length);
    }

    protected static @NotNull String copyMessage(@NotNull final String[] arguments, final int from) {
        final StringBuilder builder = new StringBuilder();

        for (int i = from; i < arguments.length; i++) {
            builder.append(arguments[i]).append(" ");
        }

        return builder.toString().trim();
    }

    public static class Note {
        private final String key;
        private final String msg;

        public Note(@NotNull final String key, @NotNull final String msg) {
            this.key = key;
            this.msg = msg;
        }

        public @NotNull String getKey() {
            return this.key;
        }

        public @NotNull String getMessage() {
            return this.msg;
        }
    }
}
