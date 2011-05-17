import play.PlayPlugin
import play.Play

import org.fusesource.scalate.scaml.ScamlOptions
import play.vfs.VirtualFile
import play.templates.Template
import play.mvc.scalate.PrecompilerProvider

package play.modules.scalate {
  class ScalatePlugin extends PlayPlugin {
    override def onConfigurationRead() {
      ScamlOptions.autoclose = ScamlOptions.autoclose.filterNot(_ == "script")
    }

    private lazy val pp = new PrecompilerProvider {

      if(play.mvc.Router.lastLoading < 0){
        play.mvc.Router.load("")
      }

      import java.io.File
      override val bytecodeDirectory: Option[File] = 
        Some(new File(new File(Play.applicationPath, "/precompiled"), "java"))

      val suffix = engine.codeGenerators.keys
    }

    private def templateFactory(file: VirtualFile): Template = new Template() {
      this.name = file.getName
      this.source = file.getRealFile.getAbsolutePath
      def compile: Unit = pp.compile(this.source)
      def internalRender(args: java.util.Map[String, AnyRef]): String = { "" } 
    } 

    override def loadTemplate(file: VirtualFile ): Template = {
      System.getProperty("precompile") match {
        case null => null
        case _ => 
          val name = file.getName
          pp.suffix.find(name.endsWith) match{
            case Some(_) => templateFactory(file)
            case _ => null
         }
      }
    }
  }
}
