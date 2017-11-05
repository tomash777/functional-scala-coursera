package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  
  def weight(tree: CodeTree): Int = tree match {
      case Leaf(x, y) => y
      case Fork(x, y, z, t) => t
    }
    
  
      def chars(tree: CodeTree): List[Char] = tree match {
        case Leaf(x, y) => List(x)
        case Fork(x, y, z, t) => z
      }

  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
    new Fork(left,right,chars(left):::chars(right),weight(left)+weight(right))
  
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def timesAcc(charsLeft: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = charsLeft match{
      case Nil => acc
      case x::xs => {
        if (acc.exists(_._1 == x)) timesAcc(xs, acc map {case (c,y) => if(c == x) (c,y+1) else (c,y)})
        else timesAcc(xs,(x,1)::acc)
      }
    }
    
   timesAcc(chars, Nil) 
   
   
  }
  
  def joinOrdered (x: (Char, Int), xs: List[Leaf]): List[Leaf] = xs match {
    case Nil => List(Leaf(x._1,x._2))
    case y::ys => if (x._2 < weight(y)) Leaf(x._1,x._2)::xs
                  else y::joinOrdered(x,ys)
  }
  
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs, z)(f))
  }
  
  
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = 
    foldRight(freqs, Nil: List[Leaf])(joinOrdered)

  def singleton(trees: List[CodeTree]): Boolean = 
    !trees.isEmpty && trees.tail.isEmpty

  def joinOrderedTrees (x: Fork, xs: List[CodeTree]): List[CodeTree] = xs match {
    case Nil => List(x)
    case y::ys => if(weight(x) < weight(y)) x::xs
                  else y:: joinOrderedTrees(x,ys) 
  }
  
  def combine(trees:List[CodeTree]): List[CodeTree] = trees match {
    case left::right::ys => combine(joinOrderedTrees(Fork(left,right,chars(left):::chars(right),weight(left)+weight(right)),ys))
    case _ => trees
  }
  
  def until(p: List[CodeTree] => Boolean, comb: List[CodeTree] => List[CodeTree])(lista: List[CodeTree]): List[CodeTree] = {
    if (p(lista)) lista else until(p,comb)(comb(lista))
  }
  
 def createCodeTree(chars: List[Char]): CodeTree = 
   until(singleton, combine)(makeOrderedLeafList(times(chars))).head


 type Bit = Int
 
 def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
   def decodeAcc(position: CodeTree, bitsleft: List[Bit], acc: List[Char]): List[Char] = position match {
     case Leaf(c,w) => if(bitsleft.isEmpty) acc++List(c)
                       else decodeAcc(tree, bitsleft,acc++List(c))
     case Fork(l,r,c,w) => bitsleft match {
                             case Nil => acc
                             case 0::xs => decodeAcc(l,bitsleft.tail,acc)
                             case 1::xs => decodeAcc(r,bitsleft.tail,acc)
                            }
   }
   decodeAcc(tree, bits, Nil)
 }
 
val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
 
def decodedSecret1: List[Char] = decode (frenchCode, secret)
 
def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeAcc(textleft:List[Char], position: CodeTree, acc: List[Bit]): List[Bit] = position match {
    case Leaf(c,w) => encodeAcc(textleft.tail, tree, acc)
    case Fork(l,r,c,w) => if(textleft.isEmpty) acc
                          else {
                            if (chars(l).contains(textleft.head)) encodeAcc(textleft,l,acc++List(0))
                            else encodeAcc(textleft,r,acc++List(1))
                          }
  }
  encodeAcc(text,tree,Nil)
}

type CodeTable = List[(Char, List[Bit])]

def codeBits(table: CodeTable)(char: Char): List[Bit] = 
  if (char == table.head._1) table.head._2
  else codeBits(table.tail)(char)
 

def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = 
  a.map{case(c,bits)=>(c,0::bits)} ++ b.map{case (c,bits)=>(c,1::bits)}

def convert(tree: CodeTree): CodeTable = tree match {
  case Leaf(c,w) => List((c,Nil))
  case Fork(l,r,c,w) => mergeCodeTables(convert(l), convert(r))
}

def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] ={
  val codeTable = convert(tree)
  text flatMap (c => codeBits(codeTable)(c))
}
  


  }
