package com.example

object State {
  def get[S]:State[S,S] = {
    State[S,S]( (s:S) => (s,s))
  }
  def put[S](s:S):State[S,Unit] = {
    State[S,Unit]( _ => ((),s))
  }

  def mkState[S,A](a:A) = State((s:S) => (a, s)) // return
 
//  def _state[S,A](f:S => (a:A,s:S)): State[S,Unit] = {
//    for {
//      s <- get[S]
//      val (a,s_) = f(s)
//      _ <- put(s_)
//      mkState(a)
//    }
//  }
}

case class State[S, A](runState: S => (A, S)){
  import com.example.State._
  def flatMap[B](f: A => State[S, B]): State[S,B] = 
    State { (s: S) =>
      runState(s) match { 
        case (v, sd) => f(v).runState(sd) 
      }
  }

  def map[B](f: A=>B):State[S,B] = flatMap((a:A) => mkState[S,B](f(a)))
  
}