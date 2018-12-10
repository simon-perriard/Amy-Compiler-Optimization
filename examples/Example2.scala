object Example2 {

  def unity1(): Unit = {
    ()
  }

  def unity2(): Unit = {
    val how: Int = 987678;
    ()
  }

  def matchInt(x: Int): Unit = { x match{
    case 5 => Std.printString("x = 5")
    case x => Std.printString("x = " ++ Std.intToString(x))
  }}

  def matchString(str: String): Unit = { str match {
    case "test" => Std.printString("This should not happen")
    case str => Std.printString("This is correct")
  }}

  def noMatchInt(x: Int): Unit = {x match{
    case 5 => Std.printString("Hello :)")
  }}

  val test1: Boolean = 90 + 80 == 170;
  val test2: Boolean = 90 - 80 == 10;
  val test3: Boolean = 90 + (-80) == 10;
  val test4: Boolean = 128 * 4 == 512;
  val test5: Boolean = 512 / 4 == 128;
  val test6: Boolean = 130 % 4 == 2;
  val test7: Boolean = !(130 % 4 == 0);
  val test8: Boolean = -8 == 2 - 10;
  val test9: Boolean = -(4*2) == 2 - 10;
  val test10: Boolean = 1 < 2;
  val test11: Boolean = 1 <= 2;
  val test12: Boolean = -1 <= 1;
  val test13: Boolean = 2 <= 2;
  val test14: Boolean = !(true && false) && (true || false);
  val test15: Boolean = true == true;
  val test16: Boolean = false == false;
  val test17: Boolean = !(true == false);
  val test18: Boolean = unity1() == unity2();
  val test19: Boolean = !("a"++"b" == "ab");
  val str: String = "Amy";
  val test20: Boolean = !(str == "Amy") && str == str;
  //val test21: Int = 1/0;

  Std.printBoolean(test1);
  Std.printBoolean(test2);
  Std.printBoolean(test3);
  Std.printBoolean(test4);
  Std.printBoolean(test5);
  Std.printBoolean(test6);
  Std.printBoolean(test7);
  Std.printBoolean(test8);
  Std.printBoolean(test9);
  Std.printBoolean(test10);
  Std.printBoolean(test11);
  Std.printBoolean(test12);
  Std.printBoolean(test13);
  Std.printBoolean(test14);
  Std.printBoolean(test15);
  Std.printBoolean(test16);
  Std.printBoolean(test17);
  Std.printBoolean(test18);
  Std.printBoolean(test19);
  Std.printBoolean(test20);


  matchInt(8);
  matchInt(5);
  matchString("test");
  matchString("hello");

  noMatchInt(0)
}