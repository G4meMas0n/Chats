package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidChannelException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Abstract moderate command representation for commands that moderates a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 19th, 2020
 * changed: June 21th, 2020
 */
public abstract class ModerateCommand extends BasicCommand {

    protected static final int CHANNEL = 0;

    protected ModerateCommand(@NotNull final String name,
                              final int minArgs,
                              final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
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

    /**
     * Executes the command for the given sender, returning its success.
     * If false is returned, then the help of the command will be sent to the sender.
     * @param sender the source who executed the command.
     * @param input the input of the sender, including used alias and passed arguments.
     * @param channel the channel that should be moderated.
     * @return true if the command execution was valid, false otherwise.
     */
    public abstract boolean execute(@NotNull final ICommandSource sender,
                                    @NotNull final ICommandInput input,
                                    @NotNull final IChannel channel) throws InputException;

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == CHANNEL + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
                    if (StringUtil.startsWithIgnoreCase(channel.getFullName(), input.get(CHANNEL))) {
                        completion.add(channel.getFullName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
