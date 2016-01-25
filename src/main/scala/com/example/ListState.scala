package com.example

object ListState{
  import com.example.State

  def getN(n:Int):State[List[Int],Int] = for {
    st1 <- State.get[List[Int]]
    _ <- State.put(st1 ++ List(n*2))
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