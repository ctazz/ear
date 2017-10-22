package org.ear

import java.net.URL
import javax.sound.sampled._

object PlayWavFile extends App {

  val url = new URL("file:///users/charlestassoni/Downloads/c-major.wav")
  val audioIn = AudioSystem.getAudioInputStream(url)
  val clip = AudioSystem.getClip
  clip.open(audioIn)
  clip.start




  Thread.sleep(3000)

}
