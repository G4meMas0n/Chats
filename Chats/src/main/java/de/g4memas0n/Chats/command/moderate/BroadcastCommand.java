package de.g4memas0n.chats.command.moderate;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;

/**
 * The broadcast command that allows to send broadcasts to channels.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: February 7th, 2020
 * changed: June 21th, 2020
 */
public final class BroadcastCommand extends ModerateCommand {

    private static final int MESSAGE = 1;

    public BroadcastCommand() {
        super("broadcast", 2, -1);

        this.setAliases(Collections.singletonList("bc"));
        this.setDescription("Broadcasts a message to a channel.");
        this.setPermission(Permission.BROADCAST.getNode());
        this.setUsage("/channel (broadcast|bc) <channel> <message>");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input,
                           @NotNull final IChannel channel) {
        if (this.argsInRange(input.getLength())) {
            this.getInstance().runSyncTask(() -> channel.performBroadcast(input.getMessage(MESSAGE)));
        }

        return false;
    }
}
