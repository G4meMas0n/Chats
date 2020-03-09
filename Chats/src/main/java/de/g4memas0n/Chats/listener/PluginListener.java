package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.util.logging.Log;
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
 * changed: February 15th, 2020
 */
public final class PluginListener extends BasicListener {

    public static final String PLUGIN_NAME_VAULT = "Vault";
    public static final String PLUGIN_NAME_HERO_CHAT = "HeroChat";

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPluginEnable(@NotNull final PluginEnableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(PLUGIN_NAME_HERO_CHAT)) {
            if (!plugin.isEnabled()) {
                return;
            }

            Log.getPluginLogger().severe("Detected unsupported plugin: HeroChat! Disabling it...");
            this.getInstance().getServer().getPluginManager().disablePlugin(event.getPlugin());

            return;
        }

        if (plugin.getName().equals(PLUGIN_NAME_VAULT)) {
            if (!plugin.isEnabled()) {
                return;
            }

            Log.getPluginLogger().info("Detected supported plugin: Vault! Setting up chat service...");

            final RegisteredServiceProvider<Chat> rsp = this.getInstance().getServer().getServicesManager()
                    .getRegistration(Chat.class);

            if (rsp == null) {
                this.getInstance().setChatService(null);
                Log.getPluginLogger().severe("Unable to setup chat service. Vault integration has been disabled.");
                return;
            }

            this.getInstance().setChatService(rsp.getProvider());
            Log.getPluginLogger().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPluginDisable(@NotNull final PluginDisableEvent event) {
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(PLUGIN_NAME_VAULT)) {
            if (plugin.isEnabled()) {
                return;
            }

            Log.getPluginLogger().severe("Detected stop of plugin Vault! Vault integration has been disabled.");
            this.getInstance().setChatService(null);
        }
    }

    @Override
    protected @NotNull String[] getRegistered() {
        return new String[]{PluginEnableEvent.class.getSimpleName(), PluginDisableEvent.class.getSimpleName()};
    }
}
