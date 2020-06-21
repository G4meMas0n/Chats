package de.g4memas0n.chats.listener;

import de.g4memas0n.chats.util.logging.Log;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.server.PluginDisableEvent;
import org.bukkit.event.server.PluginEnableEvent;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.jetbrains.annotations.NotNull;

/**
 * The Plugin Listener, listening to plugin enable and disable events, extends {@link BasicListener}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 7th, 2020
 * changed: June 17th, 2020
 */
public final class PluginListener extends BasicListener {

    public static final String VAULT = "Vault";
    public static final String HERO_CHAT = "HeroChat";

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPluginEnable(@NotNull final PluginEnableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equalsIgnoreCase(HERO_CHAT)) {
            if (!plugin.isEnabled()) {
                Log.getPlugin().severe("Detected unsupported plugin: 'HeroChat'! Plugin is already disabled.");
                return;
            }

            Log.getPlugin().severe("Detected unsupported plugin: HeroChat! Disabling it...");
            this.getInstance().getServer().getPluginManager().disablePlugin(event.getPlugin());

            return;
        }

        if (plugin.getName().equals(VAULT)) {
            if (!plugin.isEnabled()) {
                return;
            }

            Log.getPlugin().info("Detected supported plugin: Vault! Setting up chat service...");

            final RegisteredServiceProvider<Chat> rsp = this.getInstance().getServer().getServicesManager()
                    .getRegistration(Chat.class);

            if (rsp == null) {
                this.getInstance().setChatService(null);
                Log.getPlugin().severe("Unable to setup chat service. Vault integration has been disabled.");
                return;
            }

            this.getInstance().setChatService(rsp.getProvider());
            Log.getPlugin().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPluginDisable(@NotNull final PluginDisableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(VAULT)) {
            if (plugin.isEnabled()) {
                return;
            }

            this.getInstance().setChatService(null);
            Log.getPlugin().severe("Detected stop of plugin Vault! Vault integration has been disabled.");
        }
    }
}
