package de.g4memas0n.chats.command.manage;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.input.ICommandInput;
import de.g4memas0n.chats.util.input.InputException;
import de.g4memas0n.chats.util.input.InvalidPlayerException;
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
 * changed: June 22th, 2020
 */
public final class SocialSpyCommand extends BasicCommand {

    private static final int TARGET = 0;
    private static final int ENABLE = 1;

    public SocialSpyCommand() {
        super("social-spy", 0, 2);

        this.setDescription("Enables or disables social spying.");
        this.setPermission(Permission.SOCIAL_SPY.getNode());
        this.setUsage("/chats social-spy [<player> (on|off)]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) throws InputException {
        if (this.argsInRange(input.getLength())) {
            if (input.getLength() != this.getMinArgs() && input.getLength() != this.getMaxArgs()) {
                return false;
            }

            final IChatter target = input.getLength() == this.getMaxArgs()
                    ? this.getInstance().getChatterManager().getChatter(input.get(TARGET))
                    : (sender instanceof IChatter ? (IChatter) sender : null);

            if (target == null) {
                throw new InvalidPlayerException(input.getLength() == this.getMaxArgs() ? input.get(TARGET) : "CONSOLE");
            }

            final boolean enable = input.getLength() == this.getMaxArgs() ? input.getEnable(ENABLE) : !target.isSocialSpy();

            if (target.setSocialSpy(enable)) {
                sender.sendMessage(Messages.tl("spyChanged", target.getDisplayName(), Messages.tlState(enable)));
                return true;
            }

            sender.sendMessage(Messages.tl("spyAlready", target.getDisplayName(), Messages.tlState(enable)));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        if (input.getLength() == TARGET + 1) {
            final List<String> completion = new ArrayList<>();

            for (final IChatter chatter : this.getInstance().getChatterManager().getChatters()) {
                if (chatter.equals(sender) || !sender.canSee(chatter)) {
                    continue;
                }

                if (StringUtil.startsWithIgnoreCase(chatter.getName(), input.get(TARGET))) {
                    completion.add(chatter.getName());
                }
            }

            Collections.sort(completion);

            return completion;
        }

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

        return Collections.emptyList();
    }
}
