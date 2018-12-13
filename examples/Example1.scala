object Example1 {

  val l1: L.List = L.range(0, 10);
  val l2: L.List = L.range(1, 10);
  Std.printInt(L.sum(l1));
  Std.printString(L.toString(l1));
  Std.printString(L.toString(l2));
  Std.printString("Should be true : " ++ Std.booleanToString(l1 == l1));
  Std.printString("Should be false : " ++ Std.booleanToString(l1 == l2));

  val l3: L.List = L.concat(L.Cons(0, L.Nil()), l2);
  Std.printString(L.toString(l3));
  Std.printString("Should be false : " ++ Std.booleanToString(l1 == l3));

  Std.printInt(L.sum(l3))
}