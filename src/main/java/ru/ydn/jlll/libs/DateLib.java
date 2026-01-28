package ru.ydn.jlll.libs;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * Date and time primitives for working with timestamps.
 *
 * <p>
 * Provides functions for getting current time, formatting, parsing, and performing
 * date arithmetic. Timestamps are represented as milliseconds since Unix epoch (Long).
 * </p>
 *
 * <p>
 * Uses java.time API internally with system default timezone.
 * </p>
 *
 * <ul>
 * <li><b>Current time:</b> now, current-time</li>
 * <li><b>Formatting:</b> date-format, date-parse</li>
 * <li><b>Arithmetic:</b> date-add, date-diff</li>
 * <li><b>Components:</b> date-year, date-month, date-day, date-hour, date-minute, date-second,
 * date-day-of-week</li>
 * <li><b>Conversion:</b> date-&gt;list, make-date</li>
 * <li><b>Comparison:</b> date&lt;?, date&gt;?, date=?</li>
 * </ul>
 */
public class DateLib extends ReflectionLibrary
{
    /** System default timezone */
    private static final ZoneId DEFAULT_ZONE = ZoneId.systemDefault();
    /** Cache for DateTimeFormatter instances (thread-safe) */
    private static final Map<String, DateTimeFormatter> formatterCache = new ConcurrentHashMap<>();

    /**
     * Gets or creates a DateTimeFormatter for the given pattern.
     *
     * @param pattern
     *            the format pattern
     * @return the DateTimeFormatter
     * @throws JlllException
     *             if the pattern is invalid
     */
    private static DateTimeFormatter getFormatter(String pattern) throws JlllException
    {
        try
        {
            return formatterCache.computeIfAbsent(pattern, DateTimeFormatter::ofPattern);
        }
        catch (IllegalArgumentException e)
        {
            throw new JlllException("Invalid date format pattern: " + e.getMessage());
        }
    }

    /**
     * Converts a timestamp to ZonedDateTime.
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return ZonedDateTime in system timezone
     */
    private static ZonedDateTime toZonedDateTime(Long timestamp)
    {
        return Instant.ofEpochMilli(timestamp).atZone(DEFAULT_ZONE);
    }

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // date-add: Add time units to timestamp (keyword arguments)
        new Primitive("date-add", env,
                "Adds time units to a timestamp. Returns new timestamp. " + "(date-add ts :days 1) adds one day. "
                        + "(date-add ts :hours 2 :minutes 30) adds 2 hours and 30 minutes. "
                        + "Units: :years :months :weeks :days :hours :minutes :seconds :millis")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.length() < 3)
                {
                    throw new JlllException("date-add requires timestamp and at least one unit argument");
                }
                Long timestamp = ((Number) values.get(0)).longValue();
                ZonedDateTime dt = toZonedDateTime(timestamp);
                // Parse keyword arguments
                int len = values.length();
                for (int i = 1; i < len; i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= len)
                    {
                        throw new JlllException("date-add: missing value for " + key);
                    }
                    if (!(key instanceof Keyword))
                    {
                        throw new JlllException("date-add: expected keyword, got " + key);
                    }
                    String unit = ((Keyword) key).getName();
                    long amount = ((Number) values.get(i + 1)).longValue();
                    dt = addUnit(dt, unit, amount);
                }
                return dt.toInstant().toEpochMilli();
            }

            private ZonedDateTime addUnit(ZonedDateTime dt, String unit, long amount) throws JlllException
            {
                switch (unit)
                {
                    case "years" :
                        return dt.plusYears(amount);
                    case "months" :
                        return dt.plusMonths(amount);
                    case "weeks" :
                        return dt.plusWeeks(amount);
                    case "days" :
                        return dt.plusDays(amount);
                    case "hours" :
                        return dt.plusHours(amount);
                    case "minutes" :
                        return dt.plusMinutes(amount);
                    case "seconds" :
                        return dt.plusSeconds(amount);
                    case "millis" :
                        return dt.plus(amount, ChronoUnit.MILLIS);
                    default :
                        throw new JlllException("date-add: unknown unit :" + unit
                                + ". Use :years :months :weeks :days :hours :minutes :seconds :millis");
                }
            }
        };
        // date-diff: Calculate difference between timestamps in specified unit
        new Primitive("date-diff", env,
                "Calculates the difference between two timestamps in the specified unit. "
                        + "(date-diff t1 t2 :days) returns number of days between t1 and t2. "
                        + "Units: :years :months :weeks :days :hours :minutes :seconds :millis")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.length() < 3)
                {
                    throw new JlllException("date-diff requires two timestamps and a unit");
                }
                Long t1 = ((Number) values.get(0)).longValue();
                Long t2 = ((Number) values.get(1)).longValue();
                Object unitArg = values.get(2);
                if (!(unitArg instanceof Keyword))
                {
                    throw new JlllException("date-diff: third argument must be a unit keyword");
                }
                String unit = ((Keyword) unitArg).getName();
                ZonedDateTime dt1 = toZonedDateTime(t1);
                ZonedDateTime dt2 = toZonedDateTime(t2);
                return diffUnit(dt1, dt2, unit);
            }

            private Long diffUnit(ZonedDateTime dt1, ZonedDateTime dt2, String unit) throws JlllException
            {
                switch (unit)
                {
                    case "years" :
                        return ChronoUnit.YEARS.between(dt1, dt2);
                    case "months" :
                        return ChronoUnit.MONTHS.between(dt1, dt2);
                    case "weeks" :
                        return ChronoUnit.WEEKS.between(dt1, dt2);
                    case "days" :
                        return ChronoUnit.DAYS.between(dt1, dt2);
                    case "hours" :
                        return ChronoUnit.HOURS.between(dt1, dt2);
                    case "minutes" :
                        return ChronoUnit.MINUTES.between(dt1, dt2);
                    case "seconds" :
                        return ChronoUnit.SECONDS.between(dt1, dt2);
                    case "millis" :
                        return ChronoUnit.MILLIS.between(dt1, dt2);
                    default :
                        throw new JlllException("date-diff: unknown unit :" + unit
                                + ". Use :years :months :weeks :days :hours :minutes :seconds :millis");
                }
            }
        };
        // make-date: Create timestamp from components
        new Primitive("make-date", env,
                "Creates a timestamp from date/time components. "
                        + "(make-date 2024 3 15) creates March 15, 2024 at midnight. "
                        + "(make-date 2024 3 15 14 30 0) creates March 15, 2024 at 14:30:00.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                int len = values == null ? 0 : values.length();
                if (len < 3)
                {
                    throw new JlllException("make-date requires at least year, month, day");
                }
                int year = ((Number) values.get(0)).intValue();
                int month = ((Number) values.get(1)).intValue();
                int day = ((Number) values.get(2)).intValue();
                int hour = len > 3 ? ((Number) values.get(3)).intValue() : 0;
                int minute = len > 4 ? ((Number) values.get(4)).intValue() : 0;
                int second = len > 5 ? ((Number) values.get(5)).intValue() : 0;
                try
                {
                    LocalDateTime ldt = LocalDateTime.of(year, month, day, hour, minute, second);
                    return ldt.atZone(DEFAULT_ZONE).toInstant().toEpochMilli();
                }
                catch (Exception e)
                {
                    throw new JlllException("make-date: invalid date/time values - " + e.getMessage());
                }
            }
        };
    }

    /**
     * Returns the current timestamp in milliseconds since epoch.
     *
     * @return current time in milliseconds
     */
    @JlllName("now")
    public Long now()
    {
        return System.currentTimeMillis();
    }

    /**
     * Returns the current time as a hash-map with components.
     * Keys: :year, :month, :day, :hour, :minute, :second, :millis, :day-of-week
     *
     * @return hash-map with time components
     */
    @JlllName("current-time")
    public Map<Object, Object> currentTime() throws JlllException
    {
        return timestampToMap(System.currentTimeMillis());
    }

    /**
     * Formats a timestamp to a string using the specified pattern.
     *
     * <p>
     * Common patterns: "yyyy-MM-dd", "HH:mm:ss", "yyyy-MM-dd'T'HH:mm:ss"
     * </p>
     *
     * @param timestamp
     *            milliseconds since epoch
     * @param pattern
     *            format pattern (java.time.format.DateTimeFormatter syntax)
     * @return formatted date string
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("date-format")
    public String dateFormat(Number timestamp, String pattern) throws JlllException
    {
        DateTimeFormatter formatter = getFormatter(pattern);
        ZonedDateTime dt = toZonedDateTime(timestamp.longValue());
        return dt.format(formatter);
    }

    /**
     * Parses a date string to a timestamp using the specified pattern.
     *
     * @param dateStr
     *            the date string to parse
     * @param pattern
     *            format pattern
     * @return timestamp in milliseconds (Long)
     * @throws JlllException
     *             if parsing fails
     */
    @JlllName("date-parse")
    public Long dateParse(String dateStr, String pattern) throws JlllException
    {
        DateTimeFormatter formatter = getFormatter(pattern);
        try
        {
            // Try parsing as LocalDateTime first (most common)
            LocalDateTime ldt = LocalDateTime.parse(dateStr, formatter);
            return ldt.atZone(DEFAULT_ZONE).toInstant().toEpochMilli();
        }
        catch (DateTimeParseException e1)
        {
            try
            {
                // Try as LocalDate (date only, no time)
                java.time.LocalDate ld = java.time.LocalDate.parse(dateStr, formatter);
                return ld.atStartOfDay(DEFAULT_ZONE).toInstant().toEpochMilli();
            }
            catch (DateTimeParseException e2)
            {
                throw new JlllException("date-parse: cannot parse '" + dateStr + "' with pattern '" + pattern + "'");
            }
        }
    }

    /**
     * Extracts the year from a timestamp.
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return year (e.g., 2024)
     */
    @JlllName("date-year")
    public Integer dateYear(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getYear();
    }

    /**
     * Extracts the month from a timestamp (1-12).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return month (1 = January, 12 = December)
     */
    @JlllName("date-month")
    public Integer dateMonth(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getMonthValue();
    }

    /**
     * Extracts the day of month from a timestamp (1-31).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return day of month
     */
    @JlllName("date-day")
    public Integer dateDay(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getDayOfMonth();
    }

    /**
     * Extracts the hour from a timestamp (0-23).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return hour
     */
    @JlllName("date-hour")
    public Integer dateHour(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getHour();
    }

    /**
     * Extracts the minute from a timestamp (0-59).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return minute
     */
    @JlllName("date-minute")
    public Integer dateMinute(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getMinute();
    }

    /**
     * Extracts the second from a timestamp (0-59).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return second
     */
    @JlllName("date-second")
    public Integer dateSecond(Number timestamp)
    {
        return toZonedDateTime(timestamp.longValue()).getSecond();
    }

    /**
     * Extracts the day of week from a timestamp (ISO-8601: 1=Monday, 7=Sunday).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return day of week (1-7)
     */
    @JlllName("date-day-of-week")
    public Integer dateDayOfWeek(Number timestamp)
    {
        DayOfWeek dow = toZonedDateTime(timestamp.longValue()).getDayOfWeek();
        return dow.getValue(); // ISO-8601: Monday=1, Sunday=7
    }

    /**
     * Decomposes a timestamp into a list of components.
     * Returns (year month day hour minute second millis day-of-week).
     *
     * @param timestamp
     *            milliseconds since epoch
     * @return list of 8 integers
     */
    @JlllName("date->list")
    public Cons dateToList(Number timestamp)
    {
        long ts = timestamp.longValue();
        ZonedDateTime dt = toZonedDateTime(ts);
        Object[] components = new Object[]
        {dt.getYear(), dt.getMonthValue(), dt.getDayOfMonth(), dt.getHour(), dt.getMinute(), dt.getSecond(),
                (int) (ts % 1000), dt.getDayOfWeek().getValue()};
        return ListUtil.arrayToCons(components);
    }

    /**
     * Tests if timestamp t1 is before timestamp t2.
     *
     * @param t1
     *            first timestamp
     * @param t2
     *            second timestamp
     * @return true if t1 &lt; t2
     */
    @JlllName("date<?")
    public Boolean dateLessThan(Number t1, Number t2)
    {
        return t1.longValue() < t2.longValue();
    }

    /**
     * Tests if timestamp t1 is after timestamp t2.
     *
     * @param t1
     *            first timestamp
     * @param t2
     *            second timestamp
     * @return true if t1 &gt; t2
     */
    @JlllName("date>?")
    public Boolean dateGreaterThan(Number t1, Number t2)
    {
        return t1.longValue() > t2.longValue();
    }

    /**
     * Tests if two timestamps are equal.
     *
     * @param t1
     *            first timestamp
     * @param t2
     *            second timestamp
     * @return true if t1 equals t2
     */
    @JlllName("date=?")
    public Boolean dateEquals(Number t1, Number t2)
    {
        return t1.longValue() == t2.longValue();
    }

    /**
     * Converts a timestamp to a hash-map with all components.
     */
    private Map<Object, Object> timestampToMap(Long timestamp) throws JlllException
    {
        ZonedDateTime dt = toZonedDateTime(timestamp);
        Map<Object, Object> map = new LinkedHashMap<>();
        map.put(Keyword.intern("year"), dt.getYear());
        map.put(Keyword.intern("month"), dt.getMonthValue());
        map.put(Keyword.intern("day"), dt.getDayOfMonth());
        map.put(Keyword.intern("hour"), dt.getHour());
        map.put(Keyword.intern("minute"), dt.getMinute());
        map.put(Keyword.intern("second"), dt.getSecond());
        map.put(Keyword.intern("millis"), (int) (timestamp % 1000));
        map.put(Keyword.intern("day-of-week"), dt.getDayOfWeek().getValue());
        return map;
    }
}
