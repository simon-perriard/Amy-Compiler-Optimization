object Example3 {


  ~
    this function does fast exp
    @param base hello
    @param exp hedi
    @param current &benito
  ~
  def fastExp(base: Int, exp: Int, current: Int): Int = {


    if(exp == 0) { 1}

    else{
      if(exp == 1) { base * current}
      else{
        if(exp % 2 == 0){

          fastExp(base * base, exp/2, current)

        }
        else{

          fastExp(base, exp-1, current * base)
        }

      }
    }

  }

  ~
    this one does succ @see Std
  ~
  def exp(number: Int, power: Int): Int = {fastExp(number,power,1)}


  Std.printString("Enter a number :");

  val number: Int = Std.readInt();

  Std.printString("Enter a power :");

  val power: Int = Std.readInt();

  val answer: String = "The result of "++Std.intToString(number) ++ " to the power "++Std.intToString(power)++" equals : "++Std.intToString(exp(number,power));

  Std.printString(answer)

}
