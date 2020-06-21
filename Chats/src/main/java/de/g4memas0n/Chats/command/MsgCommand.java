package de.g4memas0n.chats.command;

import de.g4memas0n.chats.channel.IChannel;
import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * The Msg Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * changed: June 20th, 2020
 */
public final class MsgCommand extends BasicCommand {

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
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter chatter = (IChatter) sender;
            final IChatter partner = this.getInstance().getChatterManager().getChatter(arguments[PARTNER]);

            if (partner == null || !sender.canSee(partner)) {
                sender.sendMessage(Messages.tlErr("playerNotFound", arguments[PARTNER]));
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

                for (int i = MESSAGE; i < arguments.length; i++) {
                    message.append(arguments[i]).append(" ");
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
        if (arguments.length == PARTNER + 1) {
            if (!(sender instanceof IChatter)) {
                return Collections.emptyList();
            }

            final IChatter chatter = (IChatter) sender;
            final List<String> completion = new ArrayList<>();

            for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                if (!sender.canSee(target) || target.equals(sender) || chatter.isIgnore(target.getUniqueId())) {
                    continue;
                }

                if (sender.canMessage(target)) {
                    if (StringUtil.startsWithIgnoreCase(target.getName(), arguments[PARTNER])) {
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
