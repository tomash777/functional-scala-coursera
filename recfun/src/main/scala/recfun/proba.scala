package recfun

object proba extends App{
  
  def pascal1(r: Int, c: Int): Int = {
    if (r == c || c == 0) 1
    else pascal1(r-1,c-1) + pascal1(r-1,c)
  }
  
   
  def balance1 (chars: List[Char]): Boolean = {
     def right_opened(chars: List[Char]): Int = chars match {
      case Nil => 0
      case '('::xs => right_opened(xs) - 1
      case ')'::xs => right_opened(xs) + 1
      case x::xs => right_opened(xs)
    }
  
    def not_left_overopened(chars: List[Char]): Boolean = chars match {
      case Nil => true
      case x::xs => not_left_overopened(xs) && right_opened(chars) >=0
    }
     
    not_left_overopened(chars) && right_opened(chars)==0
  }
  
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case x::xs => x + sum(xs)
  }
  
  def countChange1(money: Int, coins: List[Int]): Int = coins match{
    case Nil => if (money == 0) 1 else 0
    case coin::rest => sum((0 to money/coin map(x => countChange1(money - x*coin, rest))).toList) 
    
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
}