package de.g4memas0n.chats.listener;

import de.g4memas0n.chats.messaging.VaultChat;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.server.PluginEnableEvent;
import org.bukkit.event.server.ServiceRegisterEvent;
import org.bukkit.event.server.ServiceUnregisterEvent;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;

/**
 * The Plugin Listener, listening for plugin enable and disable events.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public final class ServerListener extends BasicListener {

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
        }
    }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onServiceRegister(@NotNull final ServiceRegisterEvent event) {
        final Class<?> service = event.getProvider().getService();

        if (service.isAssignableFrom(Chat.class)) {
            final Plugin owner = event.getProvider().getPlugin();

            if (owner.getName().equalsIgnoreCase(VAULT)) {
                this.getInstance().getLogger().info("Detected chat service registration! Setting up chat service...");
            } else {
                this.getInstance().getLogger().info("Detected custom chat service registration! Setting up custom chat service...");
            }

            this.getInstance().setChatService(new VaultChat((Chat) event.getProvider().getProvider()));

            if (owner.getName().equalsIgnoreCase(VAULT)) {
                this.getInstance().getLogger().info("Chat service has been setup. Vault integration has been enabled.");
            } else {
                this.getInstance().getLogger().info("Custom chat service has been setup.");
            }
        }
    }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onServiceUnregister(@NotNull final ServiceUnregisterEvent event) {
        final Class<?> service = event.getProvider().getService();

        if (service.isAssignableFrom(Chat.class)) {
            final Plugin owner = event.getProvider().getPlugin();

            this.getInstance().setChatService(null);

            if (owner.getName().equalsIgnoreCase(VAULT)) {
                this.getInstance().getLogger().warning("Detected chat service unregistration! Vault integration has been disabled.");
            } else {
                this.getInstance().getLogger().warning("Detected custom chat service unregistration! Custom chat service has been disabled.");
            }
        }
    }
}
