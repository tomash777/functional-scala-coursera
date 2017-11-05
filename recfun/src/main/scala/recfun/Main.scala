package recfun

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
    if(c==0 || c==r) 1
    else pascal (c-1,r-1) + pascal(c,r-1)
  }

  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
  def N(xs: List[Char]): Int = {
    if(xs.isEmpty) 0
    else {
      if(xs.head.toString == ")") N(xs.tail) +1
      else {
        if(xs.head.toString == "(") N(xs.tail) -1
        else N(xs.tail)
      }
    }
  }

def S(xs: List[Char]): Int = {
  if(xs.isEmpty) 1
  else {
    
  
    if(S(xs.tail)==0 || N(xs)<0) 0
    else 1
  }
  }

if(N(chars)>0 || S(chars)==0) false else true
}
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
  if(money<=0 || coins.isEmpty) 0
  else 
  {
    if(coins.tail.isEmpty)
    {
      if (money % coins.head == 0) 1
      else 0       
    }
    else {
      
    def f(x: Int): Int= {
      if(x<0) 0
      else f(x-1)+ countChange(money-x*coins.head,coins.tail)
    }
    
     if ( money%coins.head ==0) f(money/(coins.head))+1 else f(money/(coins.head)) 
  }
  }
  
  } 
  }
