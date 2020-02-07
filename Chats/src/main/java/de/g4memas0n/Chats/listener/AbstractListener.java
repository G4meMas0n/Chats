package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.IChats;
import de.g4memas0n.Chats.util.ConfigKey;
import org.bukkit.event.HandlerList;
import org.bukkit.event.Listener;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract Representation of a event listener, implements {@link Listener}.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: January 6th, 2020
 * changed: January 11th, 2020
 */
public abstract class AbstractListener implements Listener {

    private IChats instance;

    protected AbstractListener() { }

    public final void register(@NotNull final IChats instance) {
        if (this.isRegistered()) {
            return;
        }

        this.instance = instance;

        this.instance.getServer().getPluginManager().registerEvents(this, instance);

        if (this.instance.getConfig().getBoolean(ConfigKey.LOG_DEBUG.getPath())) {
            this.instance.getLogger().info("ListenerHandler: Registered Listener " + this);
        }
    }

    public final void unregister() {
        if (!this.isRegistered()) {
            return;
        }

        HandlerList.unregisterAll(this);

        if (this.instance.getConfig().getBoolean(ConfigKey.LOG_DEBUG.getPath())) {
            this.instance.getLogger().info("ListenerHandler: Unregistered Listener " + this);
        }

        this.instance = null;
    }

    public final boolean isRegistered() {
        return this.instance != null;
    }

    @Override
    public final String toString() {
        return this.getClass().getSimpleName() + "{events=" + String.join(",", this.getRegistered()) + "}";
    }

    protected final @NotNull IChats getInstance() {
        if (!this.isRegistered()) {
            throw new IllegalStateException("Illegal Access! Unregistered Listener " + this
                    + " tried to get the plugin instance");
        }

        return this.instance;
    }

    protected abstract @NotNull String[] getRegistered();
}
