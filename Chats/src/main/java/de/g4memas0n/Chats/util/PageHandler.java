package de.g4memas0n.Chats.util;

import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * PageHandler, that handle multiple pages with entries of type {@link E}.
 * @param <E> the entry type of the pages of this page handler.
 *
 * @author G4meMas0n
 * @since 0.1.0-SNAPSHOT
 *
 * created: February 9th, 2020
 * changed: February 10th, 2020
 */
public class PageHandler<E> {

    private static final int PAGE_CAPACITY = 8;

    private final List<Page<E>> pages;
    private final int pageCapacity;

    private int index;

    public PageHandler() {
        this.pages = new ArrayList<>();
        this.pages.add(new Page<>(PAGE_CAPACITY));
        this.pageCapacity = PAGE_CAPACITY;
        this.index = this.pages.size() - 1;
    }

    public PageHandler(final int pageCapacity) {
        this.pages = new ArrayList<>();
        this.pages.add(new Page<>(pageCapacity));
        this.pageCapacity = pageCapacity;
        this.index = this.pages.size() - 1;
    }

    public boolean addPage(@NotNull final Page<E> page) {
        return this.pages.add(page);
    }

    public boolean addEntry(@NotNull final E entry) {
        if (this.pages.get(index).isFull()) {
            this.pages.add(new Page<>(this.pageCapacity));
            this.index = this.pages.size() - 1;
        }

        return this.pages.get(index).addEntry(entry);
    }

    public boolean removePage(@NotNull final Page<E> page) {
        final int removedIndex = this.pages.indexOf(page);

        if (this.pages.remove(page)) {
            if (removedIndex == this.index) {
                this.pages.add(new Page<>(this.pageCapacity));
                this.index = this.pages.size() - 1;
            }

            return true;
        }

        return false;
    }

    public boolean removeEntry(@NotNull final E entry) {
        for (final Page<E> current : this.pages) {
            if (current.removeEntry(entry)) {
                return true;
            }
        }

        return false;
    }

    public @NotNull Page<E> removePage(final int page) {
        final Page<E> removed = this.pages.remove(page);

        if (page == this.index) {
            this.pages.add(new Page<>(this.pageCapacity));
            this.index = this.pages.size() - 1;
        }

        return removed;
    }

    public @NotNull E removeEntry(final int page, final int index) {
        return this.pages.get(page).getEntry(index);
    }

    public @NotNull Page<E> getPage(final int page) {
        return this.pages.get(page);
    }

    public @NotNull E getEntry(final int page, final int index) {
        return this.pages.get(page).getEntry(index);
    }

    public @NotNull List<Page<E>> getPages() {
        return new ArrayList<>(this.pages);
    }

    public @NotNull List<E> getEntries() {
        final List<E> entries = new ArrayList<>(this.pages.size() * this.pageCapacity);

        for (final Page<E> current : this.pages) {
            entries.addAll(current.getEntries());
        }

        return entries;
    }

    public int getSize() {
        return this.pages.size();
    }

    public void clear() {
        this.pages.clear();
    }

    public void resort() {
        final List<E> entries = this.getEntries();
        this.clear();

        for (final E current : entries) {
            this.addEntry(current);
        }
    }
}
