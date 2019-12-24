package de.g4memas0n.Chats.listeners;

import org.bukkit.event.Listener;
import java.util.HashSet;
import java.util.Set;

public class ListenerHandler {
    private final Set<Listener> registeredListeners;

    public ListenerHandler() {
        this.registeredListeners = new HashSet<>();
    }


}
