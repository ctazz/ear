package org.ear

import javax.sound.midi.ShortMessage._
import javax.sound.midi._

object PlayMidiWithReceiverAndShortMessage extends App {

  //Notes: 20 is low A, 24 is low C, 60 is middle C

  case class NotesAndDescription(notes: Seq[Int], root: Int, desc: String)

  def nameTheNote(note: Int): String = {
    note match {
      case n if n % 12 == 0 => "C"
      case n if n % 12 == 1 => "C#"
      case n if n % 12 == 2 => "D"
      case n if n % 12 == 3 => "D#"
      case n if n % 12 == 4 => "E"
      case n if n % 12 == 5 => "F"
      case n if n % 12 == 6 => "F#"
      case n if n % 12 == 7 => "G"
      case n if n % 12 == 8 => "G#"
      case n if n % 12 == 9 => "A"
      case n if n % 12 == 10 => "BFlat"  //I like BFlat here for some reason. I know names really depend on what key you're in, but not worrying about that here
      case n if n % 12 == 11 => "B"
    }
  }


/*  def generateAll: Seq[Full] = {
    (for {
      rootTOne <- (48 to 72)
      rootNote = classifyTheNote(rootTOne)
      chordType <- Vector(Major, Minor)
      voicing <- Vector(RootPostion, FirstInversion, SecondInversion)
    } yield (Description(rootNote, chordType, voicing), rootTOne)).map{ case (description, rootTone) =>
       Full(description, PlayData(rootTone,  makeTritone(description, rootTone, false))  )

    }

  }*/

  def generateChordsInRootPosition: Seq[NotesAndDescription] = {
    for {
      root <- 48 to 72
      rootName = nameTheNote(root)
      middle <- Vector(root + 4, root + 3)
      chordType = if(middle - root == 4) "Major" else if(middle - root == 3) "Minor" else
        throw new RuntimeException(s"expected root and middle to differ by 3 or 4. Root and middle are instead $root and $middle")
      top <- Vector(root + 7)
    } yield NotesAndDescription(notes = Vector(root, middle, top), root = root, desc = s"$rootName $chordType")

  }


/*  def generateWithInversions: Seq[Seq[Int]] = {
    for {
      chordInRootPosition <- generateChordsInRootPosition
      xx <- Vector(  )
    } yield ()
    ???
  }*/

  def chooseRandomly[A](as: IndexedSeq[A]): A = {
    as(scala.util.Random.nextInt(as.size))
  }

  def raiseLowestNoteAnOctave(notesAndDescription: NotesAndDescription): NotesAndDescription = {

    notesAndDescription.copy(notes = notesAndDescription.notes.sortWith(_ < _).zipWithIndex.map{ case (note, index) =>
      (if(index == 0) note + 12 else note, index)
    }.map(_._1).sortWith(_ < _)
    )

  }

  def makeFirstInversion(notesAndDescription: NotesAndDescription): NotesAndDescription = {
    //TODO verify that notes are in root position
    raiseLowestNoteAnOctave(notesAndDescription) match {
      case notesAndDescription => notesAndDescription.copy(desc = notesAndDescription.desc + " first inversion")
    }


  }

  def makeSecondInversion(notesAndDescription: NotesAndDescription): NotesAndDescription = {
    //TODO verify that notes are in root position
    raiseLowestNoteAnOctave(raiseLowestNoteAnOctave(notesAndDescription)) match {
      case notesAndDescription => notesAndDescription.copy(desc = notesAndDescription.desc + " second inversion")
    }


  }

  def modifyByOctave(notesAndDescription: NotesAndDescription, down: Boolean = true): NotesAndDescription = {

    val change = if(down) - 12 else 12

    notesAndDescription.copy(notes = notesAndDescription.notes.map(_ + change ), root = notesAndDescription.root + change)
  }

  //TODO so far inversions only raise the pitch of some notes in the chord. I can move all notes an octave
  //lower before inverting sometines so that inversions don't always make for higher pitches.
  def randomizeInversion(notesAndDescription: NotesAndDescription): NotesAndDescription = {
    scala.util.Random.nextInt(3) match {
      case 0 => notesAndDescription
      case 1 => makeFirstInversion(notesAndDescription)
      case 2 => makeSecondInversion(notesAndDescription)
      case x => throw new RuntimeException(s"Got random number we didn't expect. It was $x and we expected numbers in the range 0..2 inclusive")
    }
  }


  object ReceiverCentricCode {
    def createMessagesDefault(notes: Int*): Seq[MidiMessage] = {
      createMessages(notes.map((_, 0, 97)))
    }

    def createMessages(notesAndChannelsAndVolumes: Seq[(Int, Int, Int)]): Seq[MidiMessage] = {

      notesAndChannelsAndVolumes.map { case (note, channel, volume) =>
        val msg = new ShortMessage //TODO haven't tried out other messages
        msg.setMessage(NOTE_ON, channel, note, volume)
        msg
      }

    }

    def playMessagesTogether(receiver: Receiver, time: Long = -1)(messages: Seq[MidiMessage]): Unit = {
      messages.foreach(receiver.send(_, time))
    }

    def playMessagesSequentially(receiver: Receiver, timeGap: Long)(messages: Seq[MidiMessage]): Unit = {
      messages.foreach{msg =>
        receiver.send(msg, -1) //TODO  Need to handle the case where the timestamp actually does something for us
        Thread.sleep(timeGap)

      }
    }

  }
  import ReceiverCentricCode._


  val rcvr = MidiSystem.getReceiver()

  val primaryCKeyChordsInRootPosition = Array( (Vector(60, 64, 67)), Vector(65, 69, 72), Vector(67, 71, 74 ) , Vector(64,67, 71), Vector(69,72, 76), Vector(62, 65, 69)  )
  //                                 cmin                     dmaj             emaj                     fmin               gmin               amaj                bmin                 bmaj
  val someOtherCommonChords = Array( Vector(60, 63, 67), Vector(62, 66, 69), Vector(64, 68, 71), Vector(65, 68, 72), Vector(67, 70, 74), Vector(69, 73, 76), Vector(71, 74, 78), Vector(71, 75, 78) )

  val notesAndDescriptionsOfChordsInCMajor: Array[NotesAndDescription] = primaryCKeyChordsInRootPosition.zip(
    Seq("C Major", "F Major", "G major", "E minor", "A minor", "D minor")
  ).map{case (notes, desc) =>
    NotesAndDescription(notes, notes.head  ,desc)
  }

  val someOtherNotesAndDescriptiosn = someOtherCommonChords.zip(
    Seq("C Minor", "D Major", "E major", "F minor", "G minor", "A major", "B minor", "B major")
  ).map{case (notes, desc) =>
    NotesAndDescription(notes, notes.head  ,desc)
  }

  val commonAndSomeOthers = notesAndDescriptionsOfChordsInCMajor ++ someOtherNotesAndDescriptiosn


  def playIt(playThis: NotesAndDescription): Unit = {
    println("playng chord")
    val messages = createMessagesDefault(playThis.notes: _*)
    playMessagesTogether(rcvr)(messages)
    //Thread.sleep(5000)
    val in1 = scala.io.StdIn.readLine()

    if (in1 == "a") {
      println("repeating")
      playIt(playThis)
    }
    else {
      println(playThis.desc + s". Notes are ${playThis.notes.sortWith(_ < _)}. Playing individual notes")
      playMessagesSequentially(rcvr, 500)(messages)
      println("playing root")
      //Thread.sleep(1000)
      playMessagesSequentially(rcvr, 500)(createMessagesDefault(playThis.root - 12))

      val in = scala.io.StdIn.readLine()
      println(s"initial user response is $in")

      if (in == "a") {
        println(s"again")
        playIt(playThis)
      }
      else if (in == "ra") {
        println("playing root")
        playMessagesSequentially(rcvr, 1000)(createMessagesDefault(playThis.root - 12))
        println("again")
        playIt(playThis)
      }
      else {
        println("next")
        Thread.sleep(1000)
      }
    }
  }

  while(true){
    //val playThis = chooseRandomly(notesAndDescriptionsOfChordsInCMajor)
    val playThis = randomizeInversion(chooseRandomly(someOtherNotesAndDescriptiosn))
    playIt(playThis)
  }

  //generateChordsInRootPosition.foreach(println(_))


  println("done")
  //Thread.sleep(1000)
}
