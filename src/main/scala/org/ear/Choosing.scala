package org.ear

object Choosing {

  def chooseRandomly[A](as: IndexedSeq[A]): A = {
    as(scala.util.Random.nextInt(as.size))
  }

  def chooseIt[A, K](as: Seq[A])(f: A => K): Seq[A] = {
    if(as.size == 1) as
    else {
      val choicesMap: Map[K, Seq[A]] = as.groupBy(f)
      choicesMap(chooseRandomly(choicesMap.keys.toIndexedSeq))
    }
  }

}
