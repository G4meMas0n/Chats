package de.g4memas0n.Chats.chatters;

import de.g4memas0n.Chats.formatters.Placeholder;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

public abstract class AbstractChatter implements IChatter {
    protected static String chatterFormat = Placeholder.SENDER.toString();
    protected final Player player;

    protected AbstractChatter(@NotNull final Player player) {
        this.player = player;
    }

    @Override
    @NotNull
    public Player getPlayer() {
        return this.player;
    }

    @Override
    public boolean addIgnoredChatter(@NotNull final IChatter chatter) {
        return this.addIgnoredChatter(chatter.getPlayer().getUniqueId());
    }

    @Override
    public boolean addIgnoredChatter(@NotNull final Player player) {
        return this.addIgnoredChatter(player.getUniqueId());
    }

    @Override
    public boolean removeIgnoredChatter(@NotNull final IChatter chatter) {
        return this.removeIgnoredChatter(chatter.getPlayer().getUniqueId());
    }

    @Override
    public boolean removeIgnoredChatter(@NotNull final Player player) {
        return this.removeIgnoredChatter(player.getUniqueId());
    }

    @Override
    public boolean isIgnoring(@NotNull final IChatter chatter) {
        return this.isIgnoring(chatter.getPlayer().getUniqueId());
    }

    @Override
    public boolean isIgnoring(@NotNull final Player player) {
        return this.isIgnoring(player.getUniqueId());
    }

    @Override
    public final boolean equals(Object object) {
        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final IChatter chatter = (AbstractChatter) object;
        return this.player.equals(chatter.getPlayer());
    }

    @NotNull
    public static String getChatterFormat() {
        return AbstractChatter.chatterFormat;
    }

    public static boolean setChatterFormat(@NotNull final String format) {
        if (AbstractChatter.chatterFormat.equals(format)) {
            return false;
        }

        AbstractChatter.chatterFormat = format;
        return true;
    }
}