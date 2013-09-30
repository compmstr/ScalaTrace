
import javax.swing.{JProgressBar, JFrame, JPanel}
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import scala.actors.{DaemonActor, Actor}

object TracerGUI {

  def viewImage(img: BufferedImage, title: String = "Ray Tracer") = {
    val frame = new JFrame(title)
    val panel = new JPanel{
      override def paintComponent(g: Graphics) = {
        g.drawImage(img, 0, 0, this)
      }
    }
    frame.add(panel)
    frame.setSize(img.getWidth, img.getHeight)
    frame.setResizable(false)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
}

class ProgressNotifier(val total: Int, val initialAmt: Int = 0) extends DaemonActor {
  var amtDone = initialAmt
  protected def onUpdate(){
  }

  def percentDone(): Double = amtDone.toDouble / total

  def reset(){
    amtDone = 0
  }

  def act() {
    while(true){
      receive {
        case num: Int =>{
          amtDone += num
          onUpdate()
        }
        case percent: Double => {
          amtDone = (total * percent).toInt
          onUpdate()
        }
      }
    }
  }
}

class SwingProgressNotifier(override val total: Int, val bar: JProgressBar, override val initialAmt: Int = 0) extends ProgressNotifier(total, initialAmt){
  bar.setMinimum(0)
  bar.setMaximum(100)
  override protected def onUpdate(){
    bar.setValue(((amtDone.toDouble / total) * 100).toInt)
  }
}
