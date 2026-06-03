package org.abh80.nf.java;

import org.abh80.nf.core.math.KinematicState;
import org.abh80.nf.core.math.Vector3D;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class KinematicsTest {

    private static final double EPS = 1e-10;

    @Test
    public void shiftedByAppliesConstantAccelKinematics() {
        Vector3D pos = Vectors.of(0.0, 0.0, 0.0);
        Vector3D vel = Vectors.of(1.0, 0.0, 0.0);
        Vector3D acc = Vectors.of(0.0, 2.0, 0.0);
        KinematicState state = Kinematics.of(pos, vel, acc);

        KinematicState moved = Kinematics.shiftedBy(state, 2.0);

        assertEquals(2.0, moved.position().x(), EPS);
        assertEquals(4.0, moved.position().y(), EPS); // 0.5 * 2 * 2^2
        assertEquals(1.0, moved.velocity().x(), EPS);
        assertEquals(4.0, moved.velocity().y(), EPS); // 0 + 2 * 2
    }

    @Test
    public void zeroIsAllZeros() {
        KinematicState zero = Kinematics.zero();
        assertEquals(0.0, zero.position().x(), EPS);
        assertEquals(0.0, zero.velocity().x(), EPS);
        assertEquals(0.0, zero.acceleration().x(), EPS);
    }

    @Test
    public void angularMomentumIsPositionCrossVelocity() {
        KinematicState state = Kinematics.fromPositionVelocity(Vectors.of(1, 0, 0), Vectors.of(0, 1, 0));
        Vector3D h = Kinematics.angularMomentum(state);
        assertEquals(0.0, h.x(), EPS);
        assertEquals(0.0, h.y(), EPS);
        assertEquals(1.0, h.z(), EPS);
    }
}
