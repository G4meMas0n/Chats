package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * Page representation, to represent a page with entries of type {@link E}.
 * @param <E> the entry type of this page.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 9th, 2020
 * changed: February 10th, 2020
 */
public class Page<E> {

    private final List<E> entries;
    private final int capacity;

    public Page() {
        this.entries = new ArrayList<>();
        this.capacity = -1;
    }

    public Page(final int capacity) {
        this.entries = new ArrayList<>(capacity);
        this.capacity = capacity;
    }

    public boolean addEntry(@NotNull final E entry) {
        if (this.isFull()) {
            return false;
        }

        this.entries.add(entry);
        return true;
    }

    public boolean removeEntry(@NotNull final E entry) {
        return this.entries.remove(entry);
    }

    public @NotNull E removeEntry(final int index) {
        return this.entries.remove(index);
    }

    public @NotNull E getEntry(final int index) {
        return this.entries.get(index);
    }

    public @NotNull List<E> getEntries() {
        return new ArrayList<>(this.entries);
    }

    public int getSize() {
        return this.entries.size();
    }

    public int getCapacity() {
        return this.capacity;
    }

    public boolean isEmpty() {
        return this.entries.isEmpty();
    }

    public boolean isFull() {
        return this.getCapacity() > 0 && this.getSize() >= this.getCapacity();
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(this.getClass().getSimpleName());

        builder.append("{capacity=");
        builder.append(this.getCapacity());
        builder.append(";size=");
        builder.append(this.getSize());
        builder.append(";entries='");

        for (int i = 0; i < this.getEntries().size(); i++) {
            if (i > 0) {
                builder.append("','");
            }

            builder.append(this.getEntry(i).toString());
        }

        builder.append("'}");

        return builder.toString();
    }
}
