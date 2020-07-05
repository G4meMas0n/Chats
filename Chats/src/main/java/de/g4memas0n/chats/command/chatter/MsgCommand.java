package de.g4memas0n.chats.command.chatter;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * The msg command that allows to start a conversation with or send a private message to a player.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: September 13th, 2019
 * changed: July 3rd, 2020
 */
public final class MsgCommand extends ChatterCommand {

    private static final int PARTNER = 0;
    private static final int MESSAGE = 1;

    public MsgCommand() {
        super("msg", 1, -1);

        this.setAliases(Arrays.asList("pm", "tell", "whisper", "w"));
        this.setDescription("Starts a conversation with or sends a private message to a player.");
        this.setPermission(Permission.MSG.getNode());
        this.setUsage("/msg <player> [<message>]");
    }

    @Override
    public boolean execute(@NotNull final IChatter sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            final IChatter partner = this.getInstance().getChatterManager().getChatter(input.get(PARTNER));

            if (partner == null || !sender.canSee(partner)) {
                throw new PlayerNotFoundException(input.get(PARTNER));
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
        if (input.getLength() == PARTNER + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                if (target.equals(sender) || sender.isIgnore(target.getUniqueId()) || !sender.canSee(target)) {
                    continue;
                }

                if (sender.canMessage(target)) {
                    if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(PARTNER))) {
                        completion.add(target.getName());
                    }
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
