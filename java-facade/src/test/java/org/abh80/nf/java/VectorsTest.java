package org.abh80.nf.java;

import org.abh80.nf.core.math.Vector3D;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class VectorsTest {

    private static final double EPS = 1e-10;

    @Test
    public void addReturnsComponentWiseSum() {
        Vector3D a = Vectors.of(1.0, 2.0, 3.0);
        Vector3D b = Vectors.of(4.0, 5.0, 6.0);
        Vector3D sum = Vectors.add(a, b);
        assertEquals(5.0, sum.x(), EPS);
        assertEquals(7.0, sum.y(), EPS);
        assertEquals(9.0, sum.z(), EPS);
    }

    @Test
    public void subtractReturnsComponentWiseDifference() {
        Vector3D a = Vectors.of(4.0, 5.0, 6.0);
        Vector3D b = Vectors.of(1.0, 2.0, 3.0);
        Vector3D diff = Vectors.subtract(a, b);
        assertEquals(3.0, diff.x(), EPS);
        assertEquals(3.0, diff.y(), EPS);
        assertEquals(3.0, diff.z(), EPS);
    }

    @Test
    public void scaleMultipliesEachComponent() {
        Vector3D v = Vectors.of(1.0, 2.0, 3.0);
        Vector3D scaled = Vectors.scale(v, 2.5);
        assertEquals(2.5, scaled.x(), EPS);
        assertEquals(5.0, scaled.y(), EPS);
        assertEquals(7.5, scaled.z(), EPS);
    }

    @Test
    public void dotMatchesEuclideanProduct() {
        Vector3D a = Vectors.of(1.0, 2.0, 3.0);
        Vector3D b = Vectors.of(4.0, -5.0, 6.0);
        assertEquals(1 * 4 + 2 * -5 + 3 * 6, Vectors.dot(a, b), EPS);
    }

    @Test
    public void crossOfStandardAxes() {
        Vector3D cross = Vectors.cross(Vector3D.PLUS_I(), Vector3D.PLUS_J());
        assertEquals(0.0, cross.x(), EPS);
        assertEquals(0.0, cross.y(), EPS);
        assertEquals(1.0, cross.z(), EPS);
    }

    @Test
    public void approxEqualsHandlesTolerance() {
        Vector3D a = Vectors.of(1.0, 2.0, 3.0);
        Vector3D b = Vectors.of(1.0 + 1e-12, 2.0, 3.0);
        assertTrue(Vectors.approxEquals(a, b));
        assertFalse(Vectors.approxEquals(a, Vectors.of(1.0 + 1e-3, 2.0, 3.0)));
    }

    @Test
    public void normalizeReturnsUnitVector() {
        Vector3D v = Vectors.of(0.0, 3.0, 4.0);
        assertEquals(1.0, Vectors.magnitude(Vectors.normalize(v)), EPS);
    }

    @Test
    public void zeroReturnsOrigin() {
        Vector3D origin = Vectors.zero();
        assertEquals(0.0, origin.x(), EPS);
        assertEquals(0.0, origin.y(), EPS);
        assertEquals(0.0, origin.z(), EPS);
    }
}
