package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;

/**
 * Tests for the DateLib date/time functions.
 */
public class DateLibTestCase
{
    private Environment env;
    // Known timestamp for testing: 2024-03-15 14:30:45.123 in system timezone
    private Long knownTimestamp;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
        // Create a known timestamp for deterministic tests
        LocalDateTime ldt = LocalDateTime.of(2024, 3, 15, 14, 30, 45);
        knownTimestamp = ldt.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli() + 123;
    }
    // ========== now ==========

    @Test
    public void testNow() throws Exception
    {
        long before = System.currentTimeMillis();
        Object result = Jlll.eval("(now)", env);
        long after = System.currentTimeMillis();
        assertTrue("now should return a Long", result instanceof Long);
        long ts = (Long) result;
        assertTrue("now should be >= test start", ts >= before);
        assertTrue("now should be <= test end", ts <= after);
    }
    // ========== current-time ==========

    @Test
    public void testCurrentTime() throws Exception
    {
        Object result = Jlll.eval("(current-time)", env);
        assertTrue("current-time should return a Map", result instanceof Map);
        @SuppressWarnings("unchecked")
        Map<Object, Object> map = (Map<Object, Object>) result;
        // Check that all expected keys are present
        assertTrue(map.containsKey(Keyword.intern("year")));
        assertTrue(map.containsKey(Keyword.intern("month")));
        assertTrue(map.containsKey(Keyword.intern("day")));
        assertTrue(map.containsKey(Keyword.intern("hour")));
        assertTrue(map.containsKey(Keyword.intern("minute")));
        assertTrue(map.containsKey(Keyword.intern("second")));
        assertTrue(map.containsKey(Keyword.intern("millis")));
        assertTrue(map.containsKey(Keyword.intern("day-of-week")));
        // Verify year is reasonable
        int year = (Integer) map.get(Keyword.intern("year"));
        assertTrue("Year should be >= 2024", year >= 2024);
    }
    // ========== date-format ==========

    @Test
    public void testDateFormatYmd() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval("2024-03-15", "(date-format ts \"yyyy-MM-dd\")");
    }

    @Test
    public void testDateFormatHms() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval("14:30:45", "(date-format ts \"HH:mm:ss\")");
    }

    @Test
    public void testDateFormatFull() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval("2024-03-15T14:30:45", "(date-format ts \"yyyy-MM-dd'T'HH:mm:ss\")");
    }

    @Test(expected = JlllException.class)
    public void testDateFormatInvalidPattern() throws Exception
    {
        Jlll.eval("(date-format (now) \"invalid{pattern}\")", env);
    }
    // ========== date-parse ==========

    @Test
    public void testDateParseYmd() throws Exception
    {
        Object result = Jlll.eval("(date-parse \"2024-03-15\" \"yyyy-MM-dd\")", env);
        assertTrue(result instanceof Long);
        Long ts = (Long) result;
        // Verify it parses to the correct date
        Jlll.eval("(define parsed " + ts + ")", env);
        eval(2024, "(date-year parsed)");
        eval(3, "(date-month parsed)");
        eval(15, "(date-day parsed)");
    }

    @Test
    public void testDateParseFull() throws Exception
    {
        Object result = Jlll.eval("(date-parse \"2024-03-15T14:30:45\" \"yyyy-MM-dd'T'HH:mm:ss\")", env);
        assertTrue(result instanceof Long);
        Long ts = (Long) result;
        Jlll.eval("(define parsed " + ts + ")", env);
        eval(14, "(date-hour parsed)");
        eval(30, "(date-minute parsed)");
        eval(45, "(date-second parsed)");
    }

    @Test(expected = JlllException.class)
    public void testDateParseInvalidInput() throws Exception
    {
        Jlll.eval("(date-parse \"not-a-date\" \"yyyy-MM-dd\")", env);
    }
    // ========== Component Extractors ==========

    @Test
    public void testDateYear() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(2024, "(date-year ts)");
    }

    @Test
    public void testDateMonth() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(3, "(date-month ts)");
    }

    @Test
    public void testDateDay() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(15, "(date-day ts)");
    }

    @Test
    public void testDateHour() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(14, "(date-hour ts)");
    }

    @Test
    public void testDateMinute() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(30, "(date-minute ts)");
    }

    @Test
    public void testDateSecond() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        eval(45, "(date-second ts)");
    }

    @Test
    public void testDateDayOfWeek() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        // March 15, 2024 is a Friday (5 in ISO-8601)
        eval(5, "(date-day-of-week ts)");
    }
    // ========== date->list ==========

    @Test
    public void testDateToList() throws Exception
    {
        Jlll.eval("(define ts " + knownTimestamp + ")", env);
        Object result = Jlll.eval("(date->list ts)", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(8, list.length());
        assertEquals(2024, list.get(0)); // year
        assertEquals(3, list.get(1)); // month
        assertEquals(15, list.get(2)); // day
        assertEquals(14, list.get(3)); // hour
        assertEquals(30, list.get(4)); // minute
        assertEquals(45, list.get(5)); // second
        assertEquals(123, list.get(6)); // millis
        assertEquals(5, list.get(7)); // day-of-week (Friday)
    }
    // ========== make-date ==========

    @Test
    public void testMakeDateYmd() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        eval(2024, "(date-year ts)");
        eval(3, "(date-month ts)");
        eval(15, "(date-day ts)");
        eval(0, "(date-hour ts)");
        eval(0, "(date-minute ts)");
    }

    @Test
    public void testMakeDateFull() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15 14 30 45))", env);
        eval(2024, "(date-year ts)");
        eval(3, "(date-month ts)");
        eval(15, "(date-day ts)");
        eval(14, "(date-hour ts)");
        eval(30, "(date-minute ts)");
        eval(45, "(date-second ts)");
    }

    @Test(expected = JlllException.class)
    public void testMakeDateInvalid() throws Exception
    {
        Jlll.eval("(make-date 2024 13 45)", env); // Invalid month/day
    }
    // ========== date-add ==========

    @Test
    public void testDateAddDays() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        Jlll.eval("(define ts2 (date-add ts :days 10))", env);
        eval(25, "(date-day ts2)");
    }

    @Test
    public void testDateAddMonths() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        Jlll.eval("(define ts2 (date-add ts :months 2))", env);
        eval(5, "(date-month ts2)");
    }

    @Test
    public void testDateAddYears() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        Jlll.eval("(define ts2 (date-add ts :years 1))", env);
        eval(2025, "(date-year ts2)");
    }

    @Test
    public void testDateAddHours() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15 10 0 0))", env);
        Jlll.eval("(define ts2 (date-add ts :hours 5))", env);
        eval(15, "(date-hour ts2)");
    }

    @Test
    public void testDateAddMultipleUnits() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15 10 0 0))", env);
        Jlll.eval("(define ts2 (date-add ts :days 1 :hours 2 :minutes 30))", env);
        eval(16, "(date-day ts2)");
        eval(12, "(date-hour ts2)");
        eval(30, "(date-minute ts2)");
    }

    @Test
    public void testDateAddNegative() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        Jlll.eval("(define ts2 (date-add ts :days -5))", env);
        eval(10, "(date-day ts2)");
    }

    @Test
    public void testDateAddWeeks() throws Exception
    {
        Jlll.eval("(define ts (make-date 2024 3 15))", env);
        Jlll.eval("(define ts2 (date-add ts :weeks 2))", env);
        eval(29, "(date-day ts2)");
    }
    // ========== date-diff ==========

    @Test
    public void testDateDiffDays() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15))", env);
        Jlll.eval("(define t2 (make-date 2024 3 25))", env);
        eval(10L, "(date-diff t1 t2 :days)");
    }

    @Test
    public void testDateDiffNegative() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 25))", env);
        Jlll.eval("(define t2 (make-date 2024 3 15))", env);
        eval(-10L, "(date-diff t1 t2 :days)");
    }

    @Test
    public void testDateDiffMonths() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 1 15))", env);
        Jlll.eval("(define t2 (make-date 2024 6 15))", env);
        eval(5L, "(date-diff t1 t2 :months)");
    }

    @Test
    public void testDateDiffYears() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2020 1 1))", env);
        Jlll.eval("(define t2 (make-date 2024 1 1))", env);
        eval(4L, "(date-diff t1 t2 :years)");
    }

    @Test
    public void testDateDiffHours() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15 10 0 0))", env);
        Jlll.eval("(define t2 (make-date 2024 3 15 15 0 0))", env);
        eval(5L, "(date-diff t1 t2 :hours)");
    }

    @Test
    public void testDateDiffMinutes() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15 10 0 0))", env);
        Jlll.eval("(define t2 (make-date 2024 3 15 10 45 0))", env);
        eval(45L, "(date-diff t1 t2 :minutes)");
    }
    // ========== Comparisons ==========

    @Test
    public void testDateLessThan() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15))", env);
        Jlll.eval("(define t2 (make-date 2024 3 16))", env);
        eval(true, "(date<? t1 t2)");
        eval(false, "(date<? t2 t1)");
        eval(false, "(date<? t1 t1)");
    }

    @Test
    public void testDateGreaterThan() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15))", env);
        Jlll.eval("(define t2 (make-date 2024 3 16))", env);
        eval(false, "(date>? t1 t2)");
        eval(true, "(date>? t2 t1)");
        eval(false, "(date>? t1 t1)");
    }

    @Test
    public void testDateEquals() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15))", env);
        Jlll.eval("(define t2 (make-date 2024 3 15))", env);
        Jlll.eval("(define t3 (make-date 2024 3 16))", env);
        eval(true, "(date=? t1 t2)");
        eval(false, "(date=? t1 t3)");
    }
    // ========== Roundtrip Tests ==========

    @Test
    public void testFormatParseRoundtrip() throws Exception
    {
        Jlll.eval("(define original (make-date 2024 6 20 15 45 30))", env);
        Jlll.eval("(define formatted (date-format original \"yyyy-MM-dd'T'HH:mm:ss\"))", env);
        Jlll.eval("(define parsed (date-parse formatted \"yyyy-MM-dd'T'HH:mm:ss\"))", env);
        eval(true, "(date=? original parsed)");
    }

    @Test
    public void testAddDiffRoundtrip() throws Exception
    {
        Jlll.eval("(define t1 (make-date 2024 3 15))", env);
        Jlll.eval("(define t2 (date-add t1 :days 100))", env);
        eval(100L, "(date-diff t1 t2 :days)");
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
