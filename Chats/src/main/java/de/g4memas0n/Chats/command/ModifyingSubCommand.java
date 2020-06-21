package de.g4memas0n.chats.command;

import de.g4memas0n.chats.IChats;
import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import java.util.List;

/**
 * Abstract modifying sub command representation for sub commands modifying channels.
 *
 * @author G4meMas0n
 * @since 0.2.4-SNAPSHOT
 *
 * created: June 20th, 2020
 * changed: June 20th, 2020
 */
public abstract class ModifyingSubCommand {

    private final ModifyingCommand parent;

    private final String name;
    private final int minArgs;
    private final int maxArgs;

    private String description;
    private String usage;

    protected ModifyingSubCommand(@NotNull final ModifyingCommand parent,
                                  @NotNull final String name,
                                  final int minArgs,
                                  final int maxArgs) {
        this.parent = parent;
        this.name = name;
        this.minArgs = minArgs;
        this.maxArgs = maxArgs;

        this.description = "";
        this.usage = "";
    }

    public final @NotNull ModifyingCommand getParent() {
        return this.parent;
    }

    public final @NotNull IChats getInstance() {
        return this.parent.getInstance();
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

    public final boolean argsInRange(final int arguments) {
        return this.maxArgs > 0
                ? arguments >= this.minArgs && arguments <= this.maxArgs
                : arguments >= this.minArgs;
    }

    /**
     * Executes the modifying sub command for the given channel, returning its success.
     * If false is returned, then the help of the modifying sub command will be sent to the sender.
     * @param sender the source who executed the sub command.
     * @param channel the channel that should be modified.
     * @param arguments the passed sub command arguments.
     * @return true if the sub command execution was valid, false otherwise.
     */
    public abstract boolean execute(@NotNull final ICommandSource sender,
                                    @NotNull final IChannel channel,
                                    @NotNull final String[] arguments);

    /**
     * Requests a list of possible completions for a sub command arguments.
     * @param sender the source who tab-completed the sub command.
     * @param channel the channel that should be modified.
     * @param arguments the passed sub command arguments, including final partial argument to be completed.
     * @return a list of possible completions for the final arguments.
     */
    public abstract @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                      @NotNull final IChannel channel,
                                                      @NotNull final String[] arguments);

    public final @NotNull String getDescription() {
        return this.description;
    }

    public final void setDescription(@NotNull final String description) {
        if (description.equals(this.description)) {
            return;
        }

        this.description = description;
    }

    public final @NotNull String getUsage() {
        return this.usage;
    }

    public final void setUsage(@NotNull final String usage) {
        if (usage.equals(this.usage)) {
            return;
        }

        this.usage = usage;
    }

    @Override
    public final @NotNull String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{parent=");
        builder.append(this.getParent().getName());
        builder.append(";name=");
        builder.append(this.getName());
        builder.append(";min-args=");
        builder.append(this.getMinArgs());
        builder.append(";max-args=");
        builder.append(this.getMaxArgs());

        if (!this.getDescription().isEmpty()) {
            builder.append(";description=");
            builder.append(this.getDescription());
        }

        if (!this.getUsage().isEmpty()) {
            builder.append(";usage=");
            builder.append(this.getUsage());
        }

        return builder.append("}").toString();
    }

    @Override
    public final boolean equals(@Nullable final Object object) {
        if (object == null) {
            return false;
        }

        if (object == this) {
            return true;
        }

        if (object instanceof ModifyingSubCommand) {
            final ModifyingSubCommand other = (ModifyingSubCommand) object;

            return this.getParent().equals(other.getParent())
                    && this.getName().equals(other.getName())
                    && this.getMinArgs() == other.getMinArgs()
                    && this.getMaxArgs() == other.getMaxArgs();
        }

        return false;
    }

    @Override
    public final int hashCode() {
        final int prime = 43;
        int result = 6;

        result = prime * result + this.getParent().hashCode();
        result = prime * result + this.getName().hashCode();
        result = prime * result + Integer.hashCode(this.getMinArgs());
        result = prime * result + Integer.hashCode(this.getMaxArgs());

        return result;
    }
}
