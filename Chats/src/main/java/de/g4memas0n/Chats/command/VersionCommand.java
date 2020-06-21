package de.g4memas0n.chats.command;

import de.g4memas0n.chats.chatter.ICommandSource;
import de.g4memas0n.chats.messaging.Messages;
import de.g4memas0n.chats.util.Permission;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginDescriptionFile;
import org.jetbrains.annotations.NotNull;
import java.util.Collections;
import java.util.List;

/**
 * The Version Command, extends {@link BasicCommand}.
 *
 * @author G4meMas0n
 * @since 0.2.0-SNAPSHOT
 *
 * created: April 17th, 2020
 * changed: June 20th, 2020
 */
public final class VersionCommand extends BasicCommand {

    public VersionCommand() {
        super("version", 0, 0);

        this.setDescription("Shows the version of this plugin.");
        this.setPermission(Permission.VERSION.getNode());
        this.setUsage("/chats version");
    }

    @Override
    public boolean execute(@NotNull final ICommandSource sender,
                           @NotNull final String alias,
                           @NotNull final String[] arguments) {
        if (this.argsInRange(arguments.length)) {
            final PluginDescriptionFile description = this.getInstance().getDescription();

            sender.sendMessage(Messages.tl("versionInfo", description.getName(), description.getVersion()));
            sender.sendMessage(Messages.tl("versionInfo", this.getInstance().getServer().getName(),
                    this.getInstance().getServer().getBukkitVersion()));

            for (final String dependency : description.getDepend()) {
                final Plugin plugin = this.getInstance().getServer().getPluginManager().getPlugin(dependency);

                if (plugin != null) {
                    sender.sendMessage(Messages.tl("versionInfo", plugin.getName(), plugin.getDescription().getVersion()));
                }
            }

            for (final String dependency : description.getSoftDepend()) {
                final Plugin plugin = this.getInstance().getServer().getPluginManager().getPlugin(dependency);

                if (plugin != null) {
                    sender.sendMessage(Messages.tl("versionInfo", plugin.getName(), plugin.getDescription().getVersion()));
                }
            }

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
