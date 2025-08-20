package org.abh80.nf
package core.time

/**
 * NavIC timescale (also called IRNWT for IRNSS NetWork Time).
 * @see [[TimeScale]]
 */
class IRNSSScale extends FixedTimeOffsetScale("IRNSS", TimeFormat(-19L, 0L))
