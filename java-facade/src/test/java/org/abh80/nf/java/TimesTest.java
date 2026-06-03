package org.abh80.nf.java;

import org.abh80.nf.core.time.AbsoluteTime;
import org.abh80.nf.core.time.TimeFormat;
import org.abh80.nf.core.time.TimeScaleFactory;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TimesTest {

    private static final double EPS = 1e-9;

    @Test
    public void ofYearMonthDayBuildsAbsoluteTime() {
        AbsoluteTime t = Times.of(2024, 1, 1, 0, 0, 0.0, TimeScaleFactory.getUTC());
        assertEquals("2024-01-01T00:00:00.000Z", t.toString());
    }

    @Test
    public void shiftedBySecondsAdvancesTime() {
        AbsoluteTime t = Times.of(2024, 1, 1, 0, 0, 0.0, TimeScaleFactory.getUTC());
        AbsoluteTime later = Times.shiftedBy(t, 60.0);
        assertEquals("2024-01-01T00:01:00.000Z", later.toString());
    }

    @Test
    public void durationBetweenMeasuresElapsed() {
        AbsoluteTime t1 = Times.of(2024, 1, 1, 0, 0, 0.0, TimeScaleFactory.getUTC());
        AbsoluteTime t2 = Times.of(2024, 1, 1, 0, 0, 30.0, TimeScaleFactory.getUTC());
        TimeFormat delta = Times.durationBetween(t2, t1);
        assertEquals(30.0, delta.toDouble(), EPS);
    }

    @Test
    public void epochIsNoArgJ2000() {
        AbsoluteTime epoch = Times.epoch();
        // J2000 = 2000-01-01 12:00 TT, which renders as 2000-01-01T11:58:55.816Z in UTC
        // (TT - UTC = 32.184 + 32 leap seconds = 64.184s at epoch).
        assertTrue(epoch.toString().startsWith("2000-01-01T11:58"));
    }
}
