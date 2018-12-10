object L {

  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  /*def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(2, concat(t, l2))
    }
  }*/
  def h(n1: Int, n2: Int): Int = {
    n1 match {
      case 2 => 42
      case _ => 36
    }
  }
}