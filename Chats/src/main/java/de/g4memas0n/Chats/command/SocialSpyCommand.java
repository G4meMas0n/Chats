package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.IChatter;
import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The SocialSpy Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.3-SNAPSHOT
 *
 * created: June 17th, 2020
 * changed: June 20th, 2020
 */
public final class SocialSpyCommand extends BasicCommand {

    private static final int BOOL = 0;

    public SocialSpyCommand() {
        super("social-spy", 0, 2);

        this.setDescription("Enables or disables social spying.");
        this.setPermission(Permission.SOCIAL_SPY.getNode());
        this.setUsage("/chats social-spy [(true|false)]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            if (!(sender instanceof IChatter)) {
                return false;
            }

            final IChatter target = (IChatter) sender;
            final Boolean bool = arguments.length == this.getMinArgs() ? Boolean.valueOf(!target.isSocialSpy())
                    : (arguments[BOOL].equalsIgnoreCase(Boolean.FALSE.toString()) ? Boolean.FALSE
                    : (arguments[BOOL].equalsIgnoreCase(Boolean.TRUE.toString()) ? Boolean.TRUE : null));

            if (bool == null) {
                sender.sendMessage(Messages.tlErr("invalidBoolean"));
                return true;
            }

            if (target.setSocialSpy(bool)) {
                target.sendMessage(Messages.tl("spyChanged", Messages.tlState(target.isSocialSpy())));
                return true;
            }

            target.sendMessage(Messages.tl("spyAlready", Messages.tlState(target.isSocialSpy())));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == BOOL + 1) {
            final List<String> completion = new ArrayList<>();

            if (StringUtil.startsWithIgnoreCase(Boolean.FALSE.toString(), arguments[BOOL])) {
                completion.add(Boolean.FALSE.toString());
            }

            if (StringUtil.startsWithIgnoreCase(Boolean.TRUE.toString(), arguments[BOOL])) {
                completion.add(Boolean.TRUE.toString());
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
