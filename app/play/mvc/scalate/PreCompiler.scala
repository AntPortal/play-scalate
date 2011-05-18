package play.mvc.scalate

import play.Play
import java.io._
import org.fusesource.scalate.DefaultRenderContext


object PreCompiler {
  def main(args: Array[String]) {
    //kick off precompiler
    val root = new File(System.getProperty("application.path"));
    Play.init(root, System.getProperty("play.id", ""));
    play.Logger.info("Precompiling scalate templates...")
    Play.start()
    val precompiler = new PrecompilerProvider()
    precompiler.precompileTemplates
  }


}

class PrecompilerProvider extends Provider{
  def reggroup = "<%@[^>]*%>".r

  val Re = "<%@.*var(.*):.*%>".r

  val compile: String => Unit = (filePath: String) => {
      val playPath = filePath.replace((new File(Play.applicationPath + "/app/views")).toString, "")
      play.Logger.info("compiling: " + playPath + " to:" + engine.bytecodeDirectory + " ...")
      val buffer = new StringWriter()
      var context = new DefaultRenderContext(null, engine, new PrintWriter(buffer))  //TODO: Nebu didn't know what the Request URI was, so he set it to null.
      // populate playcontext
      context.attributes("playcontext") = PlayContext
      //set layout
      // open file & try to find context variables and initialize them
      for (contextVariable <- reggroup findAllIn readFileToString(filePath))
        contextVariable match {
          case Re(key) => context.attributes(key.trim) = ""
          case _ =>
        }
       context.attributes("javax.servlet.error.exception") = new Exception
       context.attributes("javax.servlet.error.message") = ""
      //compile template
      try {
        engine.load(playPath)
      } catch {case ex: ClassCastException =>}
  }

  def precompileTemplates = walk(new File(Play.applicationPath, "/app/views"))(compile)

  def walk(file: File)(func: String => Unit): Boolean = {
    if (file.isFile && (file.getName.endsWith(".ssp") || file.getName.endsWith(".scaml")) && !file.getName.contains("default.ssp") && !file.getName.contains("default.scaml")) func(file.getPath)
    if (file.isDirectory) for (i <- 0 until file.listFiles.length) walk(file.listFiles()(i))(func)
    true
  }

  def readFileToString(filePath: String): String = {
    val scanLines = if (Play.configuration.getProperty("scalate.linescanned") != null) Play.configuration.getProperty("scalate.linescanned").toInt else 20
    var counter = 0
    val reader = new BufferedReader(new FileReader(filePath))
    var line: String = reader.readLine
    val sb = new StringBuffer
    while (line != null && counter != scanLines) {
      sb.append(line)
      counter = counter + 1
      line = reader.readLine()
    }
    reader.close()
    sb.toString
  }
}


