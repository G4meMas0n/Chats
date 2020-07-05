package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * The reply command that allows to start a conversation with or send a private message to the last conversation partner.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: January 11th, 2020
 * changed: July 3rd, 2020
 */
public final class ReplyCommand extends ChatterCommand {

    private static final int MESSAGE = 0;

    public ReplyCommand() {
        super("reply", 0, -1);

        this.setAliases(Collections.singletonList("r"));
        this.setDescription("Starts a conversation with or sends a private message to your last conversation partner.");
        this.setPermission(Permission.MSG.getNode());
        this.setUsage("/reply [<message>]");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) {
        if (this.argsInRange(input.getLength())) {
            final IChatter partner = sender.getLastPartner();

            if (partner == null || !sender.canSee(partner)) {
                sender.sendMessage(Messages.tl("noLastPartner"));
                return true;
            }

            if (partner.equals(sender)) {
                sender.sendMessage(Messages.tlErr("msgSelf"));
                return true;
            }

            if (sender.isIgnore(partner.getUniqueId())) {
                sender.sendMessage(Messages.tl("ignoredPartner", partner.getDisplayName()));
                return true;
            }

            if (partner.isIgnore(sender.getUniqueId()) && !sender.hasPermission(Permission.IGNORE.getChildren("bypass"))) {
                sender.sendMessage(Messages.tl("ignoredSender", partner.getDisplayName()));
                return true;
            }

            if (sender.canMessage(partner)) {
                final IChannel conversation = this.getInstance().getChannelManager().getConversation(sender, partner);

                if (input.getLength() == this.getMinArgs()) {
                    if (sender.setFocus(conversation)) {
                        sender.sendMessage(Messages.tl("focusConversation", partner.getDisplayName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("focusConversationAlready", partner.getDisplayName()));
                    return true;
                }

                this.getInstance().runSyncTask(() -> conversation.performChat(sender, input.getMessage(MESSAGE)));
                return true;
            }

            sender.sendMessage(Messages.tl("msgDenied", partner.getDisplayName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final IChatter sender,
                                             @NotNull final ICommandInput input) {
        return Collections.emptyList();
    }
}
