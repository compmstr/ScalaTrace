
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import java.awt.image.BufferedImage
import java.awt._
import scala.actors.{DaemonActor, Actor, Future}
import scala.actors.Futures._


class TracerGUI(scene: Scene, title: String = "Scala Tracer"){
  val frame = new JFrame(title)
  val progressBar = new JProgressBar(0, 100)
  var img: Option[BufferedImage] = None
  val startButton = new JButton()
  startButton.setText("Run")
  startButton.addActionListener(new ActionListener{
    def actionPerformed(e: ActionEvent) {
      //To access outer class fields, must use <OuterClass>.this. ...
      TracerGUI.this.img = None
      TracerGUI.this.panel.repaint()
      val cam: Camera = TracerGUI.this.scene.cam
      val size = cam.imageSize._1 * cam.imageSize._2
      val notifier = new SwingProgressNotifier(size, TracerGUI.this.progressBar)
      val tracer = new ScalaTrace(scene, Some(notifier))
      future {
        val rendered = tracer.rayTrace()
        TracerGUI.this.img = Some(rendered)
        TracerGUI.this.panel.repaint()
      }
    }
  })
  val panel = new JPanel{
    override def paintComponent(g: Graphics) = {
      if(img.isDefined) g.drawImage(img.get, 0, 0, this)
    }
  }
  frame.setLayout(new GridBagLayout)
  val c = new GridBagConstraints()
  panel.setSize(scene.cam.imageSize._1, scene.cam.imageSize._2)
  panel.setPreferredSize(new Dimension(scene.cam.imageSize._1, scene.cam.imageSize._2))
  c.fill = GridBagConstraints.HORIZONTAL
  c.gridwidth = 3
  c.gridx = 0
  c.gridy = 0
  frame.add(panel, c)

  c.insets = new Insets(2, 2, 2, 2)
  c.gridy = 1
  c.gridwidth = 2
  frame.add(progressBar, c)
  c.gridx = 2
  c.gridwidth = 1
  frame.add(startButton, c)
  frame.setResizable(false)
  frame.pack()
  frame.setVisible(true)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

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

  protected def onDone(){
  }

  protected def onUpdate(){
    if(amtDone == total){
      onDone()
    }
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
    super.onUpdate
    bar.setValue((percentDone * 100).toInt)
  }
}
