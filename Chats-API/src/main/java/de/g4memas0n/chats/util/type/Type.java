package de.g4memas0n.chats.util.type;

import org.jetbrains.annotations.NotNull;

/**
 * Type Interface that defines a type representation.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 */
public interface Type {

    /**
     * Returns the identifier of this type.
     *
     * @return the type identifier.
     */
    @NotNull String getIdentifier();

    /**
     * Returns the translation key of this type.
     *
     * @return the translation key.
     */
    @NotNull String getKey();
}
