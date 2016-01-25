package com.example
import com.example.State._

object GameStat {
  
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
  def current: State[Var[(Boolean,Int)], (Boolean,Int)] = 
  for(g <- get[Var[(Boolean,Int)]]) yield !g

  //Stateの中のInt値を操作する
  def addScore(i:Int): State[Var[(Boolean,Int)], (Boolean,Int)] = for {
      v <- State.get[Var[(Boolean,Int)]]
      gs = !v;
      v_ = (v := (gs._1,gs._2+i));//現在のVarから、新たなVarを作成
      _ <- put[Var[(Boolean,Int)]](v_)//新たなVarをputする
  } yield (gs._1,gs._2+i)
 
  //Stateの中のBoolean値を反転させる
  def toggle: State[Var[(Boolean,Int)], (Boolean,Int)] = for {
      v <- State.get[Var[(Boolean,Int)]]
      gs = !v;
      b = gs._1;
      v_ = (v := (!b,gs._2));//現在のVarから、新たなVarを作成
      _ <- put[Var[(Boolean,Int)]](v_)//新たなVarをputする
    } yield (!gs._1,gs._2)
    
  def playGame(s:Seq[Char]): State[Var[(Boolean,Int)], (Boolean,Int)] = {
    for {
      g <- get[Var[(Boolean,Int)]];
      v = !g;
      _ <- s match {
        case Seq('a',_*) if v._1 => addScore(1)
        case Seq('b',_*) if v._1 => addScore(-1)
        case Seq('c',_*) => toggle
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
  
    def evalState(st:State[Var[(Boolean,Int)], (Boolean,Int)], v:Var[(Boolean,Int)]) = {
        st.runState(v)
    }
  
		def main(args:Array[String]){
      val seed = new Var((false,0));
      println(playGame("ab").runState(seed)._1._2)
      println(playGame("ca").runState(seed)._1._2)
      println(playGame("cabca").runState(seed)._1._2)
      println(playGame("caaca").runState(seed)._1._2)
      println(playGame("caacbcaa").runState(seed)._1._2)
    }
}
