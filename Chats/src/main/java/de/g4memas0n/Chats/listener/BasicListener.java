package de.g4memas0n.Chats.listener;

import de.g4memas0n.Chats.IChats;
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
 * changed: February 15th, 2020
 */
public abstract class BasicListener implements Listener {

    private IChats instance;

    protected BasicListener() { }

    public final void register(@NotNull final IChats instance) {
        if (this.isRegistered()) {
            return;
        }

        this.instance = instance;

        this.instance.getServer().getPluginManager().registerEvents(this, instance);
    }

    public final void unregister() {
        if (!this.isRegistered()) {
            return;
        }

        HandlerList.unregisterAll(this);

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
            throw new IllegalStateException("Unregistered Listener " + this + " tried to get the plugin instance");
        }

        return this.instance;
    }

    protected abstract @NotNull String[] getRegistered();
}
