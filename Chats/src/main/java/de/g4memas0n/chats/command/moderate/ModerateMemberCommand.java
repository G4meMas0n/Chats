package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidChannelException;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract moderate chatter command representation for commands that moderates the chatters of channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 21th, 2020
 * changed: June 21th, 2020
 */
public abstract class ModerateMemberCommand extends ModerateCommand {

    protected static final int TARGET = 0;
    protected static final int CHANNEL = 1;

    protected ModerateMemberCommand(@NotNull final String name,
                                    final int minArgs,
                                    final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public final boolean execute(@NotNull final ICommandSource sender,
                                 @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChannel channel = this.getInstance().getChannelManager().getChannel(input.get(CHANNEL));

            if (channel == null) {
                throw new InvalidChannelException(input.get(CHANNEL));
            }

            if (sender.canModerate(channel)) {
                return this.execute(sender, input, channel);
            }

            sender.sendMessage(Messages.tl("moderateDenied", channel.getFullName()));
            return true;
        }

        return false;
    }
}
