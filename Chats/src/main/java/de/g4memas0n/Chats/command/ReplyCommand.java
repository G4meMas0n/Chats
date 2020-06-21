package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * The Reply Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 11th, 2020
 * changed: June 20th, 2020
 */
public final class ReplyCommand extends BasicCommand {

    public ReplyCommand() {
        super("reply", 0, -1);

        this.setAliases(Collections.singletonList("r"));
        this.setDescription("Starts a conversation with or sends a private message to your last conversation partner.");
        this.setPermission(Permission.MSG.getNode());
        this.setUsage("/reply [<message>]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter chatter = (IChatter) sender;
            final IChatter partner = chatter.getLastPartner();

            if (partner == null || !sender.canSee(partner)) {
                sender.sendMessage(Messages.tl("noLastPartner"));
                return true;
            }

            if (partner.equals(chatter)) {
                sender.sendMessage(Messages.tlErr("msgSelf"));
                return true;
            }

            if (chatter.isIgnore(partner.getUniqueId())) {
                sender.sendMessage(Messages.tl("ignoredPartner", partner.getDisplayName()));
                return true;
            }

            if (partner.isIgnore(chatter.getUniqueId()) && !sender.hasPermission(Permission.IGNORE.formChildren("bypass"))) {
                sender.sendMessage(Messages.tl("ignoredSender", partner.getDisplayName()));
                return true;
            }

            if (sender.canMessage(partner)) {
                final IChannel conversation = this.getInstance().getChannelManager().getConversation(chatter, partner);

                if (arguments.length == this.getMinArgs()) {
                    if (chatter.setFocus(conversation)) {
                        sender.sendMessage(Messages.tl("focusConversation", partner.getDisplayName()));
                        return true;
                    }

                    sender.sendMessage(Messages.tl("focusConversationAlready", partner.getDisplayName()));
                    return true;
                }

                final StringBuilder message = new StringBuilder();

                for (final String current : arguments) {
                    message.append(current).append(" ");
                }

                this.getInstance().runSyncTask(() -> conversation.performChat(chatter, message.toString().trim()));
                return true;
            }

            sender.sendMessage(Messages.tl("msgDenied", partner.getDisplayName()));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        return Collections.emptyList();
    }
}
