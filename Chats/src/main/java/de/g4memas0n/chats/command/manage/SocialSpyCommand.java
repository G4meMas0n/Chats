package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.PlayerNotFoundException;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The SocialSpy command, that toggle the social spying option of chatters.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: June 17th, 2020
 * changed: July 5th, 2020
 */
public final class SocialSpyCommand extends BasicCommand {

    private static final int ENABLE = 0;
    private static final int TARGET = 1;

    public SocialSpyCommand() {
        super("social-spy", 0, 2);

        this.setDescription("Enables or disables social spying.");
        this.setPermission(Permission.SOCIAL_SPY.getNode());
        this.setUsage("/chats social-spy [(on|off) [<player>]]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() != this.getMaxArgs() && !sender.isChatter()) {
                return false;
            }

            final IChatter target = input.getLength() != this.getMaxArgs() ? sender.getChatter()
                    : this.getInstance().getChatterManager().getChatter(input.get(TARGET));

            if (target == null || (!target.equals(sender) && !sender.canSee(target))) {
                throw new PlayerNotFoundException(input.get(TARGET));
            }

            final boolean enable = input.getLength() == this.getMinArgs() ? !target.isSocialSpy() : input.getEnable(ENABLE);

            if (target.setSocialSpy(enable)) {
                sender.sendMessage(Messages.tl("spyChanged", target.getDisplayName(),
                        Messages.tlState(target.isSocialSpy())));
                return true;
            }

            sender.sendMessage(Messages.tl("spyAlready", target.getDisplayName(),
                    Messages.tlState(target.isSocialSpy())));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> help(@NotNull final ICommandSource sender,
                                      @NotNull final ICommandInput input) {
        final List<String> help = new ArrayList<>();

        help.add(Messages.tl("helpHeader", this.getName()));
        help.add(Messages.tl("helpDescription", this.getDescription()));
        help.add(Messages.tl("helpUsage", sender.isChatter() ? this.getUsage()
                : this.getUsage().replaceAll("\\[(.*)]", "$1")));

        return help;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == ENABLE + 1) {
            final List<String> completion = new ArrayList<>();

            if (StringUtil.startsWithIgnoreCase(ICommandInput.ENABLE_OFF, input.get(ENABLE))) {
                completion.add(ICommandInput.ENABLE_OFF);
            }

            if (StringUtil.startsWithIgnoreCase(ICommandInput.ENABLE_ON, input.get(ENABLE))) {
                completion.add(ICommandInput.ENABLE_ON);
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
