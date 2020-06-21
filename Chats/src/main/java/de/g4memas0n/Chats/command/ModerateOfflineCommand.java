package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.chatter.IOfflineChatter;
import de.g4memas0n.chats.messaging.Messages;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * Abstract moderate command representation for moderating commands that requires an offline chatter.
 *
 * @author G4meMas0n
 * @since 0.2.4-SNAPSHOT
 *
 * created: June 19th, 2020
 * changed: June 19th, 2020
 */
public abstract class ModerateOfflineCommand extends ModerateCommand {

    protected static final int TARGET = 0;
    protected static final int CHANNEL = 1;

    protected ModerateOfflineCommand(@NotNull final String name,
                                     final int minArgs,
                                     final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public final boolean execute(@NotNull final ICommandSource sender,
                                 @NotNull final String alias,
                                 @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(arguments[TARGET]);

            if (target == null) {
                sender.sendMessage(Messages.tlErr("playerNotFound", arguments[TARGET]));
                return true;
            }

            final IChannel channel = this.getInstance().getChannelManager().getChannel(arguments[CHANNEL]);

            if (channel == null || channel.isConversation()) {
                sender.sendMessage(Messages.tlErr("channelNotExist", arguments[CHANNEL]));
                return true;
            }

            if (sender.canModerate(channel)) {
                return this.execute(sender, target, channel);
            }

            sender.sendMessage(Messages.tl("moderateDenied", channel.getColoredName()));
            return true;
        }

        return false;
    }

    /**
     * Executes the moderate command for the given offline chatter and the given channel, returning its success.
     * If false is returned, then the help of the moderate command will be sent to the sender.
     * @param sender the source who executed the command.
     * @param target the offline chatter that should be moderated.
     * @param channel the channel in that the offline chatter should be moderated.
     * @return true if the command execution was valid, otherwise false.
     */
    public abstract boolean execute(@NotNull final ICommandSource sender,
                                    @NotNull final IOfflineChatter target,
                                    @NotNull final IChannel channel);

    @Override
    public final @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                   @NotNull final String alias,
                                                   @NotNull final String[] arguments) {
        if (arguments.length == TARGET + 1) {
            return this.tabComplete(sender, arguments[TARGET]);
        }

        if (arguments.length == CHANNEL + 1) {
            final IOfflineChatter target = this.getInstance().getChatterManager().getOfflineChatter(arguments[TARGET]);

            if (target == null) {
                return Collections.emptyList();
            }

            return this.tabComplete(sender, target, arguments[CHANNEL]);
        }

        return Collections.emptyList();
    }

    /**
     * Requests a list of possible player completions for the commands first argument.
     * @param sender the source who tab-completed the command.
     * @param target the name that possible players must have.
     * @return a list of possible completions for the final player argument.
     */
    public abstract @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                      @NotNull final String target);

    /**
     * Requests a list of possible channel completions for the commands second argument.
     * @param sender the source who tab-completed the command.
     * @param target the chatter that should be moderated.
     * @param channel the name that possible channels must have.
     * @return a list of possible completions for the final channel argument.
     */
    public abstract @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                                      @NotNull final IOfflineChatter target,
                                                      @NotNull final String channel);
}
