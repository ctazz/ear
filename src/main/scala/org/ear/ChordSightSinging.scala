package org.ear
object ChordSightSinging extends App {


  def choose[A](vec: Vector[A], history: Vector[A], nextIsOk: (A, A) => Boolean = { (_: A, _: A) => true}): A = {
    val next = vec(
      scala.util.Random.nextInt(vec.size))

    history.lastOption match {
      case None =>
        //println(s"we're just starting out, so we accept every next. next is $next")
        next
      case Some(prev) =>
        //println(s"history is $history")
        if(nextIsOk(prev, next)) {
          //println(s"next was accepted. Next is $next. Prev was $prev")
          next
        } else  {
          //println(s"next was not accepted. Discarded next was $next and prev was $prev")
          choose(vec, history, nextIsOk)
        }
    }


  }

  val chords =   Vector("I", "ii", "iii", "IV", "V", "vi")
  //val chords =Vector("C", "D", "E", "F", "G", "A" ,"B", "C" ) //
  val voicings = Vector("root", "3", "5")


  //G  A B C D E F G
  def noteName(n: Int): String = {
    n match {
      case -3 => "G-Low"
      case -2 => "A-Low"
      case -1 => "Bee-Low"
      case 0 => "C"
      case 1 => "D"
      case 2 => "E"
      case 3 => "F"
      case 4 => "G"
      case 5 => "A"
      case 6 => "Bee"
      case 7 => "C-High"
      case 8 => "D-High"
      case 9 => "E-High"
      case 10 => "F-High"
      case 11 => "G-High"
      case other => throw new RuntimeException(s"bad int is $n")
    }
  }

  def takeNNotes(n: Int): Vector[String] = {
    //val noteNums = (-3 to 11).toVector
    val noteNums = (0 to 7 ).toVector
    def loop(currNum: Int, noteHistory: Vector[Int]): Vector[String] = {
      if(currNum == n) {
        println(s"final noteHistory is $noteHistory")
        noteHistory.map(noteName)
      }
      else {
        val nextNote = choose[Int](noteNums, noteHistory, { (prev, next) =>
          //println(s"takeNotes: In descision function, prev is $prev and next candidate is $next")
          val distance = Math.abs(prev - next)

          //if(distance > 2 || distance == 0){
           if( distance > 5 || distance <= 1) {
               //println(s"takeNotes: prev is $prev and next is $next and distance is $distance. Decision is false")
             false
           }
           else {
             //println(s"TakeNotes: prev is $prev and next is $next and distance is $distance. Decision is true")
             true
           }

        }

        )
        loop(currNum + 1, noteHistory :+ nextNote)
      }
    }

    loop(0, Vector.empty)

  }


  def take(n: Int): Vector[String] = {

    def loop(currNum: Int, chordHistory: Vector[String], voicingHistory: Vector[String]): Vector[String] = {
      if(currNum == n) {
        //println(s"in end section: chordHistory=$chordHistory,voicingHistory=$voicingHistory")
        chordHistory.zip(voicingHistory).map { case (chord, voicing) => s"$chord-$voicing" }
      }
      else {
        //Want to avoid two of the same chords in a row, but don't care about consecutive voicings that are the same, since
        //they'll be over different chords
       // println("in main section")
        val nextChord = choose[String](chords, chordHistory, { (prev, next) => prev != next  })
        val nextVoicing = choose( voicings, Vector.empty)
        loop(currNum = currNum + 1, chordHistory = chordHistory :+ nextChord, voicingHistory = voicingHistory :+ nextVoicing)
      }
    }


    loop(0, Vector.empty, Vector.empty)
  }
  println(
    take(4).mkString(",    "))

/*  println(
  takeNNotes(4).mkString(",    "))*/

}

