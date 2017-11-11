import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "A List" should "add to head" in {
    val l = Nil
    val l2 = 4 :: l
    l2.head should be (4)

    val l3 = 77 :: l2
    l3.head should be (77)
  }

  it should "construct from something" in {
    val l2 = 4 :: List(42)
    l2.head should be (4)

    val l3 = 77 :: l2
    l3.head should be (77)
  }

  it should "construct from multiple values" in {
    val l2 = 4 :: List(42, 17)
    l2.head should be(4)
  }
}
