object Example4 {

  //"Imports"


  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List


  def head(l: List): Int = {
    l match {
      case Cons(h, _) => h
      case Nil() => error("head(Nil)")
    }
  }

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }

  def reverse(l: List): List = {
    reverseAcc(l, Nil())
  }

  def reverseAcc(l: List, acc: List): List = {
    l match {
      case Nil() => acc
      case Cons(h, t) => reverseAcc(t, Cons(h, acc))
    }
  }


  def tail(l: List): Int = {head(reverse(l))}

  //Test 1 equality

  val s1: String = "asd";
  val s2: String = s1;

  if(s1 == s2){
    Std.printString("Test 1: OK")
  }
  else{
    Std.printString("Test 1: FAIL")
  };

  //Test 2 list concatenation

  val l: List = Cons(2,Nil());
  val l2: List = Cons(4,Nil());

  val l3: List = concat(l,l2);
  if(head(l3) == 2 && tail(l3) == 4){
    Std.printString("Test 2: OK")
  }
  else{
    Std.printString("Test 2: FAIL")
  }


}