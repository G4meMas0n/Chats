package de.g4memas0n.Chats.utils;

import org.jetbrains.annotations.NotNull;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

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
            throw new IllegalArgumentException("Invalid Map! Map must contain keys '" + MAP_KEY_AUTHOR + "', '"
                    + MAP_KEY_MESSAGE + "' and '" + MAP_KEY_TIME + "'.");
        }

        try {
            this.author = content.get(MAP_KEY_AUTHOR).toString();
            this.message = content.get(MAP_KEY_MESSAGE).toString();
            this.timeMillis = Long.parseLong(content.get(MAP_KEY_TIME).toString());
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("Invalid Map! Key 'time' must be a parsable long.");
        }
    }

    @NotNull
    public Date getTime() {
        return new Date(this.timeMillis);
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
    public Map<?, ?> toMap() {
        HashMap<String, Object> noticeMap = new HashMap<>();

        noticeMap.put(MAP_KEY_AUTHOR, this.author);
        noticeMap.put(MAP_KEY_MESSAGE, this.message);
        noticeMap.put(MAP_KEY_TIME, this.timeMillis);

        return noticeMap;
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
    public int compareTo(@NotNull final Notice notice) {
        return Long.compare(this.timeMillis, notice.timeMillis);
    }
}