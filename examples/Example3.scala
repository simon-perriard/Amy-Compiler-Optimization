object Example3 {


  ~
  This is the (tail-recursive) helper function for the @see Example3.exp function
  @param base the base of the power
  @param exp the power
  @param current the result of the previous computations
  ~

  def fastExp(base: Int, exp: Int, current: Int): Int = {


    if (exp == 0) {
      1
    }

    else {
      if (exp == 1) {
        base * current
      }
      else {
        if (exp % 2 == 0) {

          fastExp(base * base, exp / 2, current)

        }
        else {

          fastExp(base, exp - 1, current * base)
        }

      }
    }

  }

  ~This is an implementation of the fast exponentiation algorithm.
   This algorithm works in __O(log(n))__
  @param number the base
  @param power the power
  @return the result of the number^power
  ~

  def exp(number: Int, power: Int): Int = {
    fastExp(number, power, 1)
  }


  Std.printString("Enter a number :");

  val number: Int = Std.readInt();

  Std.printString("Enter a power :");

  val power: Int = Std.readInt();

  val answer: String = "The result of " ++ Std.intToString(number) ++ " to the power " ++ Std.intToString(power) ++ " equals : " ++ Std.intToString(exp(number, power));

  Std.printString(answer)

}
