package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.command.InputException;
import de.g4memas0n.chats.command.PlayerNotFoundException;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;
import static de.g4memas0n.chats.messaging.Messages.tlState;

/**
 * The SocialSpy command, that toggle the social spying option of chatters.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class SocialSpyCommand extends BasicCommand {

    private static final int ENABLE = 0;
    private static final int TARGET = 1;

    public SocialSpyCommand() {
        super("social-spy", 0, 2);

        this.setPermission(Permission.SOCIAL_SPY.getNode());
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() == this.getMaxArgs()) {
                final IChatter target = this.getInstance().getChatterManager().getChatter(input.get(TARGET));

                if (target == null || !sender.canSee(target)) {
                    throw new PlayerNotFoundException(input.get(TARGET));
                }

                if (!target.equals(sender)) {
                    if (target.setSocialSpy(input.getBoolean(ENABLE))) {
                        sender.sendMessage(tl("spyChangedOther", target.getDisplayName(), tlState(target.isSocialSpy())));
                        return true;
                    }

                    sender.sendMessage(tl("spyAlreadyOther", target.getDisplayName(), tlState(target.isSocialSpy())));
                    return true;
                }
            }

            if (sender instanceof IChatter) {
                final IChatter target = (IChatter) sender;

                if (target.setSocialSpy(input.getLength() == this.getMinArgs() ? !target.isSocialSpy() : input.getBoolean(ENABLE))) {
                    sender.sendMessage(tl("spyChanged", tlState(target.isSocialSpy())));
                    return true;
                }

                sender.sendMessage(tl("spyAlready", tlState(target.isSocialSpy())));
                return true;
            }

            sender.sendMessage(this.getDescription());
            sender.sendMessage(this.getUsage().replaceAll("\\[(.*)]", "$1"));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == ENABLE + 1) {
            final List<String> completion = new ArrayList<>();

            if (StringUtil.startsWithIgnoreCase("disable", input.get(ENABLE))) {
                completion.add("disable");
            }

            if (StringUtil.startsWithIgnoreCase("enable", input.get(ENABLE))) {
                completion.add("enable");
            }

            if (StringUtil.startsWithIgnoreCase("off", input.get(ENABLE))) {
                completion.add("off");
            }

            if (StringUtil.startsWithIgnoreCase("on", input.get(ENABLE))) {
                completion.add("on");
            }

            return completion;
        }

        if (input.getLength() == TARGET + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChatter target : this.getInstance().getChatterManager().getChatters()) {
                if (target.equals(sender) || !sender.canSee(target)) {
                    continue;
                }

                if (StringUtil.startsWithIgnoreCase(target.getName(), input.get(TARGET))) {
                    completion.add(target.getName());
                }
            }

            Collections.sort(completion);

            return completion;
        }

        return Collections.emptyList();
    }
}
