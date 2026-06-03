package org.abh80.nf.java;

import org.abh80.nf.core.time.Date;
import org.abh80.nf.core.time.TimeScale;
import org.abh80.nf.core.time.TimeScaleFactory;
import org.abh80.nf.core.time.UTCScale;
import org.junit.Test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;

public class LeapSecondsTest {

    @Test
    public void createBuildsLeapSecondOffsetWithExplicitTAI() {
        TimeScale tai = TimeScaleFactory.getTAI();
        Date start = Date.apply(2020, 1, 1);

        UTCScale.LeapSecondOffset offset = LeapSeconds.create(
            tai, start, start.getMJD(), 38.0, 0.0
        );

        assertNotNull(offset);
        assertEquals(38.0, offset.fixedOffset(), 0.0);
        assertEquals(start.getMJD(), offset.mjdBase());
    }
}
