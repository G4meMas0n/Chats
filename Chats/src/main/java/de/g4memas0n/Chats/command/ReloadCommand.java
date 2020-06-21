package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import de.g4memas0n.chats.util.type.ReloadType;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The Reload Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 13th, 2020
 * changed: June 20th, 2020
 */
public final class ReloadCommand extends BasicCommand {

    private static final int TYPE = 0;

    public ReloadCommand() {
        super("reload", 0, 1);

        this.setDescription("Reloads this plugin.");
        this.setPermission(Permission.RELOAD.getNode());
        this.setUsage("/chats reload [(all|channels|chatters|config)]");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final ReloadType type = arguments.length == this.getMinArgs() ? ReloadType.getDefault()
                    : ReloadType.getType(arguments[TYPE]);

            if (type == null) {
                sender.sendMessage(Messages.tlErr("invalidType"));
                return true;
            }

            if (sender.canReload(type)) {
                if (type == ReloadType.ALL) {
                    this.getInstance().reloadConfig();
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();

                    sender.sendMessage(Messages.tl("reloadCompleted", this.getInstance().getDescription().getName()));
                    return true;
                } else if (type == ReloadType.CONFIG) {
                    this.getInstance().reloadConfig();
                } else if (type == ReloadType.CHANNELS) {
                    this.getInstance().getChannelManager().load();
                    this.getInstance().getChatterManager().load();
                } else if (type == ReloadType.CHATTERS) {
                    this.getInstance().getChatterManager().load();
                }

                sender.sendMessage(Messages.tl("reloadCompleted", Messages.tlType(type)));
                return true;
            }

            sender.sendMessage(Messages.tl("reloadDenied", Messages.tlType(type)));
            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final String alias,
                                             @NotNull final String[] arguments) {
        if (arguments.length == TYPE + 1) {
            final List<String> completion = new ArrayList<>();

            for (final ReloadType type : ReloadType.values()) {
                if (sender.canReload(type)) {
                    if (StringUtil.startsWithIgnoreCase(type.getIdentifier(), arguments[TYPE])) {
                        completion.add(type.getIdentifier());
                    }
                }
            }

            return completion;
        }

        return Collections.emptyList();
    }
}
