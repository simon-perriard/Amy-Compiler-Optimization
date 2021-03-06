object L {

  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  ~Checks wether a list is empty
  @param l a list
  @return true if the list is empty, false otherwise
  ~
  def isEmpty(l: List): Boolean = {
    l match {
      case Nil() => true
      case _ => false
    }
  }

  ~Returns the size a given list
  @param l a list
  @return the length of the list
  ~
  def length(l: List): Int = {
    l match {
      case Nil() => 0
      case Cons(_, t) => 1 + length(t)
    }
  }

  ~Returns the head of the list given in parameters or an error
  @param l a list
  @return the head of the list or an error if the list is empty
  ~
  def head(l: List): Int = {
    l match {
      case Cons(h, _) => h
      case Nil() => error("head(Nil)")
    }
  }

  ~Returns the head of the list **if any**
  @param l a list
  @return an option (@see O) containing the head
  of the list if any, the get method (@see O.isDefined)
  will be needed to check whether the option contains
  something
  ~
  def headOption(l: List): O.Option = {
    l match {
      case Cons(h, _) => O.Some(h)
      case Nil() => O.None()
    }
  }

  ~Reverses this list
  @param l a list
  @return the reversed list
  ~
  def reverse(l: List): List = {
    reverseAcc(l, Nil())
  }

  ~Reverses a list and concatenate it after a second list
  @param l the list to _**be reversed**_
  @param acc the list that will come **before** the reversed list
  @return the first list reversed concatenated to the second list
  ~
  def reverseAcc(l: List, acc: List): List = {
    l match {
      case Nil() => acc
      case Cons(h, t) => reverseAcc(t, Cons(h, acc))
    }
  }

  ~Finds the index of an element in the list
  @param l the list to search in
  @param i the integer you're looking for
  @return the index of the element if found, -1 otherwise
  ~
  def indexOf(l: List, i: Int): Int = {
    l match {
      case Nil() => -1
      case Cons(h, t) =>
        if (h == i) {
          0
        }
        else {
          val rec: Int = indexOf(t, i);
          if (0 <= rec) {
            rec + 1
          }
          else {
            -1
          }
        }
    }
  }

  def range(from: Int, to: Int): List = {
    if (to < from) {
      Nil()
    }
    else {
      Cons(from, range(from + 1, to))
    }
  }

  def sum(l: List): Int = {
    l match {
      case Nil() => 0
      case Cons(h, t) => h + sum(t)
    }
  }

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }

  def contains(l: List, elem: Int): Boolean = {
    l match {
      case Nil() =>
        false
      case Cons(h, t) =>
        h == elem || contains(t, elem)
    }
  }

  abstract class LPair

  case class LP(l1: List, l2: List) extends LPair

  def merge(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h1, t1) =>
        l2 match {
          case Nil() => l1
          case Cons(h2, t2) =>
            if (h1 <= h2) {
              Cons(h1, merge(t1, l2))
            } else {
              Cons(h2, merge(l1, t2))
            }
        }
    }
  }

  def split(l: List): LPair = {
    l match {
      case Cons(h1, Cons(h2, t)) =>
        val rec: LPair = split(t);
        rec match {
          case LP(rec1, rec2) =>
            LP(Cons(h1, rec1), Cons(h2, rec2))
        }
      case _ =>
        LP(l, Nil())
    }
  }

  def mergeSort(l: List): List = {
    l match {
      case Nil() => l
      case Cons(h, Nil()) => l
      case l =>
        split(l) match {
          case LP(l1, l2) =>
            merge(mergeSort(l1), mergeSort(l2))
        }
    }
  }

  def toString(l: List): String = {
    l match {
      case Nil() => "List()"
      case more => "List(" ++ toString1(more) ++ ")"
    }
  }

  def toString1(l: List): String = {
    l match {
      case Cons(h, Nil()) => Std.intToString(h)
      case Cons(h, t) => Std.intToString(h) ++ ", " ++ toString1(t)
    }
  }

  ~This function returns the n first elements of a list
  @param l a list
  @param n the number of element wanted
  @return the first n elements of the list
  ~
  def take(l: List, n: Int): List = {
    if (n <= 0) {
      Nil()
    }
    else {
      l match {
        case Nil() => Nil()
        case Cons(h, t) =>
          Cons(h, take(t, n - 1))
      }
    }
  }

}
