package org.ear

object Keyboard {

  def offset(char: Char): Int = {
    char match {
      case 'q' => -4
      case 'a' => -3
      case 'w' => -2
      case 's' => -1
      case 'd' => 0
      case 'r' => 1
      case 'f' => 2
      case 't'  => 3
      case 'g' => 4
      case 'h' => 5
      case 'u' => 6
      case 'j' => 7
      case 'i' => 8
      case 'k' => 9
      case 'o' => 10
      case 'l' => 11
      case ';' => 12
      case '[' => 13
      case ''' => 14
      case ']' => 15
      case other => throw new RuntimeException(s"$other does not correspond to a valid musical note")
    }
  }


}