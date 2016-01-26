package com.example
import com.example.State._

object ListState{

  def getN(n:Int):State[List[Int],Int] = for {
    st1 <- get[List[Int]]
    _ <- put(st1 ++ List(n*2))
  } yield n*2

  def mkListValueST = (
    for{ n1 <- getN(1);
		   n2 <- getN(n1)
		} yield (n1, n2)).runState(_)

  def main(args:Array[String]){
    val l = mkListValueST(List(0));
    println(l)
  }
}