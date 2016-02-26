package com.example
import com.example.State._

object GameStat {
  
  //ステータスクラス
  case class GameState(b:Boolean, score:Int) {}
  
  //入れ物クラス
  class Var[T](c: T) {
    //新しいVarを取得
    def :=(newContent: T): Var[T] = new Var(newContent)
    //Varの中身を取得
    def unary_! : T = c
    //println用
    override def toString(): String = "State(" + c.toString + ")"
  }

  //現在のStateを返す
  def current: State[Var[GameState], GameState] = 
  for(g <- get[Var[GameState]]) yield !g

  //Stateの中のInt値を操作する
  def addScore(i:Int): State[Var[GameState], GameState] = for {
      v <- State.get[Var[GameState]]
      gs = !v;
      v_ = v := GameState(gs.b,gs.score+i);//現在のVarから、新たなVarを作成
      _ <- put[Var[GameState]](v_)//新たなVarをputする
  } yield GameState(gs.b,gs.score+i)
 
  //Stateの中のBoolean値を反転させる
  def toggle: State[Var[GameState], GameState] = for {
      v <- State.get[Var[GameState]]
      gs = !v;
      b = gs.b;
      v_ = (v := GameState(!b,gs.score));//現在のVarから、新たなVarを作成
      _ <- put[Var[GameState]](v_)//新たなVarをputする
    } yield GameState(!gs.b,gs.score)
  
  //実行
  def playGame(s:Seq[Char]): State[Var[GameState], GameState] = {
    for {
      g <- get[Var[GameState]];
      v = !g;
      _ <- s match {
        case Seq('a', tail @ _*) if v.b => addScore(1)
        case Seq('b', tail @ _*) if v.b => addScore(-1)
        case Seq('c', tail @ _*) => toggle
        case _ => current
      }
      r <- s match {
        //文字がまだ有るならば、再帰実行
        case Seq(_, tail @ _*) => playGame(tail)
        //文字が無ければ、終了
        case _ => current
      }
    } yield r
  }
  
    def main(args:Array[String]){
      // aは+1、bは-1、cはトグルスイッチ、スイッチがtrueじゃないと加算減算できない。
      val seed = new Var(GameState(false,0));
      println(evalState[Var[GameState],GameState](playGame("caacbcaa"), seed).score)
      println(playGame("ab").runState(seed)._1.score)
      println(playGame("ca").runState(seed)._1.score)
      println(playGame("cabca").runState(seed)._1.score)
      println(playGame("caaca").runState(seed)._1.score)
      println(playGame("caacbcaa").runState(seed)._1.score)
    }
}
