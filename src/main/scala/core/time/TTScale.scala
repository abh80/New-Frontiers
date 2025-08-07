package org.abh80.nf
package core.time

class TTScale extends FixedTimeOffsetScale("TT", TimeFormat.fromTimeUnit(32L, TimeUnit.SECONDS) + TimeFormat.fromTimeUnit(184L, TimeUnit.MILLISECONDS))