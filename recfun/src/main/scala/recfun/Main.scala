package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    
    def next( curC :Int, curR :Int, curRow :IndexedSeq[Int], prevRow :IndexedSeq[Int] ) :Int = {
      def calc = { 
	      if ( prevRow.isDefinedAt(curC - 1) && prevRow.isDefinedAt(curC) )
	        prevRow( curC - 1 ) + prevRow(curC)
	      else 
	        1
      }

      if ( curR == r  && curC == c ) 
        calc
      else if ( curC == curR ) 
        next( 0, curR + 1, IndexedSeq.empty[Int] , curRow :+ calc )
      else 
        next( curC + 1, curR, curRow :+ calc, prevRow )
    }

    next( 0, 0, IndexedSeq.empty[Int], IndexedSeq.empty[Int])
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def calc( cnt :Int, remaining :List[Char] ) :Boolean = {
      
      def calcChar = remaining.head.toString match {
        case "(" => 1 + cnt
        case ")" => -1 + cnt
        case _ => cnt
      }
      
      if ( cnt < 0 )
        false
      else if ( remaining.isEmpty && cnt == 0 )
        true
      else if ( remaining.isEmpty && cnt != 0 )
        false
      else
        calc( calcChar , remaining.tail )
    }
    
    calc(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ( money == 0 )
      1
    else if ( money < 0 || coins.isEmpty )
      0
    else
      countChange( money, coins.tail ) + countChange( money - coins.head, coins )
  }
}
