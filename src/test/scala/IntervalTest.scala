import org.scalatest.FunSuite
import svlis.lib._

class IntervalTest extends FunSuite {
  test("Interval.member") {
    val iv = new Interval(3, 6)
    assert(iv.member(5) === MemTest.Solid)
    assert(iv.member(2) === MemTest.Air)
    assert(iv.member(3) === MemTest.Surface)
    assert(iv.member(6) === MemTest.Surface)
  }

  test("Interval.pow") {
    assert(new Interval(-4.0, -2.0).pow(2) === new Interval(4.0, 16.0))
    assert(new Interval(-1.0, 1.0).pow(2) === new Interval(0.0, 1.0))
    assert(new Interval(2.0, 4.0).pow(2) === new Interval(4.0, 16.0))
    assert(new Interval(-2.0, 4.0).pow(2) === new Interval(0.0, 16.0))

    assert(new Interval(-4.0, -2.0).pow(3) === new Interval(-64.0, -8.0))
    assert(new Interval(-2.0, 4.0).pow(3) === new Interval(-8, 64.0))
    assert(new Interval(-5.0, 4.0).pow(3) === new Interval(-125.0, 64.0))
    assert(new Interval(2.0, 4.0).pow(3) === new Interval(8.0, 64.0))
  }
}
