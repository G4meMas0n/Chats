package de.g4memas0n.chats.listener;

import net.milkbowl.vault.chat.Chat;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.server.PluginDisableEvent;
import org.bukkit.event.server.PluginEnableEvent;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.jetbrains.annotations.NotNull;

/**
 * The Plugin Listener, listening for plugin enable and disable events.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class PluginListener extends BasicListener {

    public static final String VAULT = "Vault";
    public static final String HERO_CHAT = "HeroChat";

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPluginEnable(@NotNull final PluginEnableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equalsIgnoreCase(HERO_CHAT)) {
            if (!plugin.isEnabled()) {
                this.getInstance().getLogger().severe("Detected unsupported plugin: 'HeroChat'! Plugin is already disabled.");
                return;
            }

            this.getInstance().getLogger().severe("Detected unsupported plugin: HeroChat! Disabling it...");
            this.getInstance().getServer().getPluginManager().disablePlugin(event.getPlugin());

            return;
        }

        if (plugin.getName().equalsIgnoreCase(VAULT)) {
            if (!plugin.isEnabled()) {
                return;
            }

            this.getInstance().getLogger().info("Detected supported plugin: Vault! Setting up chat service...");

            final RegisteredServiceProvider<Chat> rsp = this.getInstance().getServer().getServicesManager()
                    .getRegistration(Chat.class);

            if (rsp == null) {
                this.getInstance().setChatService(null);

                this.getInstance().getLogger().severe("Unable to setup chat service. Vault integration has been disabled.");

                return;
            }

            this.getInstance().setChatService(rsp.getProvider());
            this.getInstance().getLogger().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPluginDisable(@NotNull final PluginDisableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(VAULT)) {
            if (plugin.isEnabled()) {
                return;
            }

            this.getInstance().setChatService(null);
            this.getInstance().getLogger().severe("Detected stop of plugin Vault! Vault integration has been disabled.");
        }
    }
}
