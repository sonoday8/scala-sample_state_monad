package com.example

object State {
  def get[S]:State[S,S] = {
    State[S,S]( (s:S) => (s,s))
  }
  def put[S](s:S):State[S,Unit] = {
    State[S,Unit]( _ => ((),s))
  }

  def mkState[S,A](a:A):State[S,A] = State((s:S) => (a, s)) // return
 
  def evalState[S,A](s:State[S,A], a:S):A = {
      s.runState(a)._1
  }
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