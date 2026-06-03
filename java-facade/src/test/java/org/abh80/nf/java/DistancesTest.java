package org.abh80.nf.java;

import org.abh80.nf.core.metrics.DistanceUnit;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class DistancesTest {

    private static final double EPS = 1e-9;

    @Test
    public void kilometersConvertToMeters() {
        DistanceUnit.Kilometer km = Distances.kilometers(1.5);
        assertEquals(1500.0, km.toMeter(), EPS);
    }

    @Test
    public void astronomicalUnitsConvertToMeters() {
        DistanceUnit.AstronomicalUnit au = Distances.astronomicalUnits(1.0);
        assertEquals(1.495978707e11, au.toMeter(), 1.0);
    }

    @Test
    public void addSumsDistances() {
        DistanceUnit sum = Distances.add(Distances.meters(500.0), Distances.meters(500.0));
        assertEquals(1000.0, sum.toMeter(), EPS);
    }

    @Test
    public void scaleByScalar() {
        DistanceUnit scaled = Distances.scale(Distances.kilometers(2.0), 0.5);
        assertEquals(1000.0, scaled.toMeter(), EPS);
    }

    @Test
    public void ratioIsDimensionless() {
        assertEquals(2.0, Distances.ratio(Distances.meters(100.0), Distances.meters(50.0)), EPS);
    }
}
