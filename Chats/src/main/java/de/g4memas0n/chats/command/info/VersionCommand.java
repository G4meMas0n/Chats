package de.g4memas0n.chats.command.info;

import de.g4memas0n.chats.command.BasicCommand;
import de.g4memas0n.chats.command.ICommandInput;
import de.g4memas0n.chats.command.ICommandSource;
import de.g4memas0n.chats.permission.Permission;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

import static de.g4memas0n.chats.messaging.Messages.tl;

/**
 * The version command that allows to show the version of this plugin and the dependencies.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class VersionCommand extends BasicCommand {

    public VersionCommand() {
        super("version", 0, 0);

        this.setDescription("Shows the version of this plugin.");
        this.setPermission(Permission.VERSION.getNode());
        this.setUsage("/chats version");
    }

    @Override
    public boolean hide(@NotNull final ICommandSource sender) {
        return false;
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final ICommandInput input) {
        if (this.argsInRange(input.getLength())) {
            sender.sendMessage(tl("versionInfo", this.getInstance().getName(),
                    this.getInstance().getDescription().getVersion()));
            sender.sendMessage(tl("versionInfo", this.getInstance().getServer().getName(),
                    this.getInstance().getServer().getBukkitVersion()));

            for (final String dependency : this.getInstance().getDescription().getDepend()) {
                final Plugin plugin = this.getInstance().getServer().getPluginManager().getPlugin(dependency);

                if (plugin != null) {
                    sender.sendMessage(tl("versionInfo", plugin.getName(), plugin.getDescription().getVersion()));
                }
            }

            for (final String dependency : this.getInstance().getDescription().getSoftDepend()) {
                final Plugin plugin = this.getInstance().getServer().getPluginManager().getPlugin(dependency);

                if (plugin != null) {
                    sender.sendMessage(tl("versionInfo", plugin.getName(), plugin.getDescription().getVersion()));
                }
            }

            return true;
        }

        return false;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull final ICommandSource sender,
                                             @NotNull final ICommandInput input) {
        return Collections.emptyList();
    }
}
