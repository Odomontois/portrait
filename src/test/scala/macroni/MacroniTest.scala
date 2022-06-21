package macroni

import potrait.Macroni

trait Foo:
  type X
  def a(s: String): Int
  def b(x: X)(z: List[X]): X

trait Bar[A[_]]:
  def u(s: String): Int
  def b(uu: List[Int]): A[String]

@main def MacroniTes() =
  println(Macroni.lol[Foo])

  println(Macroni.lol[Bar])
