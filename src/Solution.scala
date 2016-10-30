import math._
import scala.collection.mutable.{ArrayBuffer, ListBuffer, StringBuilder}
import scala.util._

/**
  * Created by wael on 10/29/16.
  */
/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Solution extends App {

  def isBracketingCharater(c : Char) : Boolean = c match{
    case '{' => true
    case '}' => true
    case '(' => true
    case ')' => true
    case '[' => true
    case ']' => true
    case '<' => true
    case '>' => true
    case _ => false
  }

  def matches(c : Char, d : Char) : Boolean = c match {
    case '{' => d == '{' || d == '}'
    case '}' => d == '{' || d == '}'
    case '(' => d == '(' || d == ')'
    case ')' => d == '(' || d == ')'
    case '[' => d == '[' || d == ']'
    case ']' => d == '[' || d == ']'
    case '<' => d == '<' || d == '>'
    case '>' => d == '<' || d == '>'
  }

  def cleanExpression(stringExpression : String) : String  = {
    val exitString = new StringBuilder()
    val entryString = new StringBuilder(stringExpression)
    while(!entryString.isEmpty){
      if(isBracketingCharater(entryString.charAt(0))) exitString += entryString.charAt(0)
      entryString.deleteCharAt(0)
    }
    exitString.toString()
  }

  def eval(stringExp: String, depth : Int = 0) : Boolean ={
    if(stringExp.isEmpty) return true
    if(stringExp.length % 2 == 1 ) return false
    if(System.currentTimeMillis - startTime > 5000) return false
    //System.err.println("  " * depth + "Full: " + stringExp.length)
    val firstChar = stringExp charAt 0
    var stringRemainder : String = ""
    var foundMatching = false
    for(i <- 1 until stringExp.length by 2 if !foundMatching){
      foundMatching = matches(firstChar, stringExp.charAt(i))
      if(foundMatching && eval(stringExp.substring(1, i), depth+1))
        stringRemainder = stringExp.substring(i+1, stringExp.length)
      else
        foundMatching = false
    }
    foundMatching && eval(stringRemainder, depth)
  }

  val startTime = System.currentTimeMillis
  val n = readInt
  val strings = new Array[String](n)
  for(i <- 0 until n)
    strings(i) = cleanExpression(readLine)
  for(i <- 0 until n)
    println(eval(strings(i)))
}