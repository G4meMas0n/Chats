package de.g4memas0n.chats.util;

import org.jetbrains.annotations.NotNull;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Representation of a Notice, that provides author, time, and message methods of notices for easier notice handling.
 * Currently unused.
 *
 * @author G4meMas0n
 * @since Release 1.0.0
 *
 * created: July 5th, 2019
 * changed: June 25th, 2020
 */
@SuppressWarnings("unused")
public class Notice implements Comparable<Notice> {
    private static final String MAP_KEY_AUTHOR = "author";
    private static final String MAP_KEY_MESSAGE = "message";
    private static final String MAP_KEY_TIME = "time";

    private final String author;
    private final String message;
    private final long timeMillis;

    public Notice(@NotNull final String author, @NotNull final String message) {
        this.author = author;
        this.message = message;
        this.timeMillis = System.currentTimeMillis();
    }

    public Notice(@NotNull final String author, @NotNull final String message, final long timeMillis) {
        this.author = author;
        this.message = message;
        this.timeMillis = timeMillis;
    }

    public Notice(@NotNull final Map<?, ?> content) throws IllegalArgumentException {
        if (!content.containsKey(MAP_KEY_AUTHOR)
                || !content.containsKey(MAP_KEY_MESSAGE)
                || !content.containsKey(MAP_KEY_TIME)) {
            throw new IllegalArgumentException(String.format("Map must contain keys: %s, %s, %s",
                    MAP_KEY_AUTHOR, MAP_KEY_MESSAGE, MAP_KEY_TIME));
        }

        try {
            this.author = content.get(MAP_KEY_AUTHOR).toString();
            this.message = content.get(MAP_KEY_MESSAGE).toString();
            this.timeMillis = Long.parseLong(content.get(MAP_KEY_TIME).toString());
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException(String.format("Key '%s' must be a parsable long", MAP_KEY_TIME));
        }
    }

    @NotNull
    public String getAuthor() {
        return this.author;
    }

    @NotNull
    public String getMessage() {
        return this.message;
    }

    @NotNull
    public Date getTime() {
        return new Date(this.timeMillis);
    }

    @NotNull
    public Map<?, ?> toMap() {
        HashMap<String, Object> noticeMap = new HashMap<>();

        noticeMap.put(MAP_KEY_AUTHOR, this.author);
        noticeMap.put(MAP_KEY_MESSAGE, this.message);
        noticeMap.put(MAP_KEY_TIME, this.timeMillis);

        return noticeMap;
    }

    @Override
    public int compareTo(@NotNull final Notice notice) {
        return Long.compare(this.timeMillis, notice.timeMillis);
    }

    @Override
    public String toString() {
        return "Notice{author=" + this.getAuthor() + ";message=" + this.getMessage() + ";time=" + this.getTime() + "}";
    }

    @Override
    public boolean equals(@NotNull final Object object) {
        if (this == object) {
            return true;
        }

        if (this.getClass() != object.getClass()) {
            return false;
        }

        final Notice notice = (Notice) object;
        return this.timeMillis == notice.timeMillis
                && this.author.equals(notice.author)
                && this.message.equals(notice.message);
    }

    @Override
    public int hashCode() {
        final int prime = 47;
        int result = 2;

        result = prime * result + this.getAuthor().hashCode();
        result = prime * result + this.getMessage().hashCode();
        result = prime * result + this.getTime().hashCode();

        return result;
    }
}
