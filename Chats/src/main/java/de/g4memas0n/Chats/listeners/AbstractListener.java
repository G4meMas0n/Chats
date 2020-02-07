package de.g4memas0n.Chats.listeners;

import de.g4memas0n.Chats.Chats;
import de.g4memas0n.Chats.IChats;
import org.bukkit.event.HandlerList;
import org.bukkit.event.Listener;
import org.jetbrains.annotations.NotNull;

public abstract class AbstractListener implements Listener {

    private static final String REGISTER_FAILURE = "Failed to register Listener '%s': %s.";
    private static final String REGISTER_SUCCESS = "Listener '%s' successfully registered.";
    private static final String UNREGISTER_FAILURE = "Failed to unregister Listener '%s': %s.";
    private static final String UNREGISTER_SUCCESS = "Listener '%s' successfully unregistered.";

    private IChats instance;

    AbstractListener() {

    }

    public final void register(@NotNull final IChats instance) {
        final boolean debug = instance.getConfigManager().isLogDebug();

        if (this.instance != null) {
            this.instance.getLogger().warning(String.format(REGISTER_FAILURE, this.getClass().getSimpleName(), "Listener already registered"));
            return;
        }

        this.instance = instance;

        instance.getServer().getPluginManager().registerEvents(this, instance);

        if (debug) {
            this.instance.getLogger().info(String.format(REGISTER_SUCCESS, this.getClass().getSimpleName()));
        }
    }

    public final void unregister() {
        if (this.instance == null) {
            final IChats tempInstance = Chats.getInstance();
            if (tempInstance != null) {
                tempInstance.getLogger().warning(String.format(UNREGISTER_FAILURE, this.getClass().getSimpleName(), "TabExecutor already unregistered"));
            }
            return;
        }

        final boolean debug = this.instance.getConfigManager().isLogDebug();

        HandlerList.unregisterAll(this);

        if (debug) {
            this.instance.getLogger().info(String.format(UNREGISTER_SUCCESS, this.getClass().getSimpleName()));
        }

        this.instance = null;
    }
}
