package org.abh80.nf.java;

import org.abh80.nf.core.metrics.AngleUnit;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class AnglesTest {

    private static final double EPS = 1e-12;

    @Test
    public void degreesConvertToRadians() {
        AngleUnit.Degree deg = Angles.degrees(180.0);
        assertEquals(Math.PI, deg.toRadians(), EPS);
    }

    @Test
    public void radiansFactoryRoundTrips() {
        AngleUnit.Radian rad = Angles.radians(1.5);
        assertEquals(1.5, rad.toRadians(), EPS);
    }

    @Test
    public void arcSecondsToRadians() {
        AngleUnit.ArcSecond as = Angles.arcSeconds(3600.0);
        assertEquals(Math.PI / 180.0, as.toRadians(), EPS);
    }

    @Test
    public void addSumsAcrossUnits() {
        AngleUnit sum = Angles.add(Angles.degrees(90.0), Angles.degrees(90.0));
        assertEquals(Math.PI, sum.toRadians(), EPS);
    }

    @Test
    public void ratioIsDimensionless() {
        double r = Angles.ratio(Angles.degrees(180.0), Angles.degrees(90.0));
        assertEquals(2.0, r, EPS);
    }

    @Test
    public void approxEqualsDefaultTolerance() {
        assertTrue(Angles.approxEquals(Angles.radians(1.0), Angles.radians(1.0 + 1e-12)));
    }
}
