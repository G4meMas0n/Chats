package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.IChats;
import net.milkbowl.vault.chat.Chat;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.server.PluginDisableEvent;
import org.bukkit.event.server.PluginEnableEvent;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.jetbrains.annotations.NotNull;

/**
 * The Plugin Listener, listening to plugin enable and disable events, extends {@link AbstractListener}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 7th, 2020
 * changed: January 7th, 2020
 */
public final class PluginListener extends AbstractListener {

    public static final String PLUGIN_NAME_VAULT = "Vault";
    public static final String PLUGIN_NAME_HERO_CHAT = "HeroChat";

    public PluginListener() { }

    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPluginEnable(@NotNull final PluginEnableEvent event) {
        final IChats instance = this.getInstance();
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(PLUGIN_NAME_HERO_CHAT)) {
            if (plugin.isEnabled()) {
                return;
            }

            instance.getLogger().severe("Detected unsupported plugin HeroChat! Disabling it...");
            this.getInstance().getServer().getPluginManager().disablePlugin(event.getPlugin());

            return;
        }

        if (plugin.getName().equals(PLUGIN_NAME_VAULT)) {
            if (!plugin.isEnabled()) {
                return;
            }

            instance.getLogger().info("Detected supported plugin: 'Vault'! Setting up chat service...");

            final RegisteredServiceProvider<Chat> rsp = this.getInstance().getServer().getServicesManager()
                    .getRegistration(Chat.class);

            if (rsp == null) {
                this.getInstance().getChannelManager().getFormatter().setChatService(null);
                instance.getLogger().severe("Failed to setup chat service. Vault integration has been disabled.");
                return;
            }

            this.getInstance().getChannelManager().getFormatter().setChatService(rsp.getProvider());
            instance.getLogger().info("Chat service has been set up. Vault integration has been enabled.");
        }
    }

    @EventHandler(priority = EventPriority.NORMAL)
    public void onPluginDisable(@NotNull final PluginDisableEvent event) {
        final IChats instance = this.getInstance();
        final Plugin plugin = event.getPlugin();

        if (plugin.getName().equals(PLUGIN_NAME_VAULT)) {
            if (plugin.isEnabled()) {
                return;
            }

            instance.getLogger().severe("Detected stop of plugin Vault! Vault integration has been disabled.");
            this.getInstance().getChannelManager().getFormatter().setChatService(null);
        }
    }

    @Override
    protected @NotNull String[] getRegistered() {
        return new String[]{PluginEnableEvent.class.getSimpleName(), PluginDisableEvent.class.getSimpleName()};
    }
}
