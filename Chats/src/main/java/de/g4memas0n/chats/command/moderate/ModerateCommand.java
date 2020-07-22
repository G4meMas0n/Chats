package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandSource;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract moderate command representation for commands that moderates a channel.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public abstract class ModerateCommand extends BasicCommand {

    protected static final int TARGET = 0;
    protected static final int CHANNEL = 1;

    protected ModerateCommand(@NotNull final String name,
                              final int minArgs,
                              final int maxArgs) {
        super(name, minArgs, maxArgs);
    }

    @Override
    public final boolean hide(@NotNull final ICommandSource sender) {
        if (sender instanceof IChatter) {
            for (final IChannel channel : this.getInstance().getChannelManager().getChannels()) {
                if (channel.isConversation()) {
                    continue;
                }

                if (sender.canModerate(channel)) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }
}
