package macroni

import potrait.Macroni
import scala.annotation.experimental
import scala.annotation.nowarn

trait Foo:
  type X
  val z: Int = 2
  def a(s: String): Int
  def b(x: X)(z: List[X]): X
  def b1(x: X): X = b(x)(Nil)
  final def b2(x: X): X = b(x)(List(x))

trait Bar[A[_]]:
  def u(s: String): Int
  def b(uu: List[Int]): A[String]

@nowarn
@main def MacroniTest() =

  println(Macroni.lol[Foo])

  println(Macroni.lol[Bar])
