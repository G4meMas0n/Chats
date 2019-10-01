package de.g4memas0n.Chats.chatter;

import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

/**
 * Abstract representation of a chatter, implements the {@link IChatter} interface.
 *
 * @author G4meMas0n
 * @since 0.0.1-SNAPSHOT
 *
 * created: August 6th, 2019
 * last change: October 1st, 2019
 */
public abstract class AbstractChatter implements IChatter {

    protected final Player player;

    protected AbstractChatter(@NotNull final Player player) {
        this.player = player;
    }

    @Override
    public @NotNull Player getPlayer() {
        return this.player;
    }

    @Override
    public final boolean addIgnores(@NotNull final IChatter chatter) {
        return this.addIgnores(chatter.getPlayer().getUniqueId());
    }

    @Override
    public final boolean addIgnores(@NotNull final Player player) {
        return this.addIgnores(player.getUniqueId());
    }

    @Override
    public final boolean removeIgnores(@NotNull final IChatter chatter) {
        return this.removeIgnores(chatter.getPlayer().getUniqueId());
    }

    @Override
    public final boolean removeIgnores(@NotNull final Player player) {
        return this.removeIgnores(player.getUniqueId());
    }

    @Override
    public final boolean isIgnoring(@NotNull final IChatter chatter) {
        return this.isIgnoring(chatter.getPlayer().getUniqueId());
    }

    @Override
    public final boolean isIgnoring(@NotNull final Player player) {
        return this.isIgnoring(player.getUniqueId());
    }
}
