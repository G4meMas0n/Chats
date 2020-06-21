package de.g4memas0n.chats.chatter;

import de.g4memas0n.chats.storage.IStorageHolder;
import org.jetbrains.annotations.NotNull;
import java.util.Set;
import java.util.UUID;

/**
 * IOfflineChatter Interface that defines a offline chatter representation, extends {@link IStorageHolder}.
 *
 * @author G4meMas0n
 * @since 0.2.1-SNAPSHOT
 *
 * created: May 9th, 2020
 * changed: June 18th, 2020
 */
public interface IOfflineChatter extends IStorageHolder {

    /**
     * Returns the uniqueId of this (offline) chatter.
     * @return the chatter's uniqueId.
     */
    @NotNull UUID getUniqueId();

    /**
     * Returns the name of this (offline) chatter.
     * @return the chatter's name.
     */
    @NotNull String getName();

    /**
     * Returns the last time in milliseconds the (offline) chatter has played.
     * When the chatter is currently online, it returns the current time.
     * @return the last playtime in milliseconds.
     */
    long getLastPlayed();

    /**
     * Returns whether this (offline) chatter is muted or not.
     * @return true when the chatter is muted, false otherwise.
     */
    boolean isMuted();

    /**
     * Sets whether this (offline) chatter is muted or not.
     * @param muted true to mute and false to unmute.
     * @return true when the chatter was muted or unmuted as result of this call, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean setMuted(final boolean muted);

    /**
     * Returns whether this (offline) chatter is social spying or not.
     * @return true when the chatter is social spying, false otherwise.
     */
    boolean isSocialSpy();

    /**
     * Sets whether this (offline) chatter is social spying or not.
     * @param enabled true to enabled social spying, false to disabled.
     * @return true when social spying was enabled or disabled, false otherwise.
     */
    boolean setSocialSpy(final boolean enabled);

    /**
     * Returns all ignored chatters of this (offline) chatter.
     * @return the uniqueIds of the ignored chatters.
     */
    @NotNull Set<UUID> getIgnores();

    /**
     * Sets the ignored chatters for this (offline) chatter.
     * @param ignores the uniqueIds of the chatters.
     * @return true when the ignored chatters has changed as result of this call, false otherwise.
     */
    @SuppressWarnings("unused")
    boolean setIgnores(@NotNull final Set<UUID> ignores);
}
