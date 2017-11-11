import org.scalatest.FunSuite
import svlis._

class IntervalTest extends FunSuite {
  test("Interval.member") {
    val iv = new Interval(3, 6)
    assert(iv.member(5) === MemTest.Solid)
    assert(iv.member(2) === MemTest.Air)
    assert(iv.member(3) === MemTest.Surface)
    assert(iv.member(6) === MemTest.Surface)
  }
}

