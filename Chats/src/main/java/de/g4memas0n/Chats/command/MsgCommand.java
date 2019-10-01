package de.g4memas0n.Chats.command;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Msg Command Handler, extends {@link AbstractCommand}.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: September 13th, 2019
 * last change: October 1st, 2019
 */
@org.bukkit.plugin.java.annotation.command.Command(name = "msg", desc = "Sends a private message to another chatter.", aliases = {"pm", "whisper", "w"}, permission = "chats.msg")
public class MsgCommand extends AbstractCommand {

    private static final String PERMISSION = "chats.msg";
    private static final int MAX_PARAM = Integer.MAX_VALUE;

    public MsgCommand() {
        super(PERMISSION, MAX_PARAM);
    }

    @Override
    public boolean onCommand(@NotNull final CommandSender sender,
                             @NotNull final Command command,
                             @NotNull final String alias,
                             @NotNull final String[] arguments) {
        //TODO Implement onCommand method for the msg-command.
        return false;
    }

    @Override
    public @NotNull List<String> onTabComplete(@NotNull final CommandSender sender,
                                               @NotNull final Command command,
                                               @NotNull final String alias,
                                               @NotNull final String[] arguments) {
        //TODO Implement obTabComplete method for the msg-command.
        return new ArrayList<>();
    }
}
