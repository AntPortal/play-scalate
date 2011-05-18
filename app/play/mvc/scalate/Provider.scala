package play.mvc.scalate

import play.classloading.enhancers.LocalvariablesNamesEnhancer.LocalVariablesNamesTracer
import play.Play
import play.exceptions.{UnexpectedException,PlayException,TemplateNotFoundException}
import play.data.validation.Validation
import org.fusesource.scalate._
import org.fusesource.scalate.util.FileResourceLoader
import java.io.{StringWriter,PrintWriter}
import java.io.{File}
import org.fusesource.scalate.util.SourceCodeHelper
import play.vfs.{VirtualFile => VFS}
import scala.collection.JavaConversions._
import play.mvc.results.ScalateResult
import play.mvc.{Scope, Http}

trait Provider {
  
  def renderWithScalate(templateName: String = null, args: Seq[(Symbol, Any)] = Seq[(Symbol, Any)]()){
    //determine template
    val _templateName:String =
        if(templateName!=null){
          templateName
        }
        else if (args.length > 0 && args(0).isInstanceOf[String] &&
          LocalVariablesNamesTracer.getAllLocalVariableNames(args(0)).isEmpty) {
          discardLeadingAt(args(0).toString)
        } else {
          determineURI()
        }
    if (shouldRenderWithScalate(_templateName)) {
      renderScalateTemplate(_templateName, args)
    } else {
      throw new RuntimeException("could not find scalate template")
    }
  }

  val bytecodeDirectory: Option[File] = None

  // Create and configure the Scalate template engine
  //TODO: Nebu removed the import to models._ because this directory does not always exist.
  private def initEngine(usePlayClassloader:Boolean = true, customImports: String="import controllers._;import play.utils._" ):TemplateEngine = {
    val engine = new TemplateEngine {
      override def bytecodeDirectory = 
        Provider.this.bytecodeDirectory.getOrElse(super.bytecodeDirectory)
    }

    val playcontext = SourceCodeHelper.name(PlayContext.getClass).dropRight(1)+".type"
    engine.bindings = List(
      Binding("context", SourceCodeHelper.name(classOf[DefaultRenderContext]), true),
      Binding("playcontext", playcontext, true)
    )
    engine.importStatements ++= List(customImports)
    if (Play.mode == Play.Mode.PROD && Play.configuration.getProperty("scalate.allowReloadInProduction") == null ) engine.allowReload = false

    engine.workingDirectory = new File(Play.applicationPath,"/tmp")

    engine.resourceLoader = new FileResourceLoader(Some(new File(Play.applicationPath+"/app/views")))
    val classpathRoot = if (new File(Play.applicationPath,"/precompiled/java").exists && Play.mode == Play.Mode.PROD)
                            new File(Play.applicationPath,"/precompiled/java").toString
                        else
                            new File(Play.applicationPath,"/tmp/classes").toString
    engine.classpath = classpathRoot
    engine.combinedClassPath = true
    val renderMode = Play.configuration.getProperty("scalate")
    engine.layoutStrategy = new layout.DefaultLayoutStrategy(engine,"/default."+renderMode) 
    if (usePlayClassloader) engine.classLoader = Play.classloader
    engine
  }
  val engine = initEngine()
  
  def defaultTemplate = Http.Request.current().action
  def requestFormat = Http.Request.current().format 
  def controller = Http.Request.current().controller
  def validationErrors = Validation.errors

 


  
  
  private def errorTemplate:String = {
    val fullPath = new File(Play.applicationPath,"/app/views/errors/500.scaml").toString 
    fullPath.replace(new File(Play.applicationPath+"/app/views").toString,"")
  }
  
  private def renderScalateTemplate(templateName:String, args:Seq[(Symbol, Any)]) {
    val renderMode = Play.configuration.getProperty("scalate")
    //loading template
    val buffer = new StringWriter()
    var context = new DefaultRenderContext(null, engine, new PrintWriter(buffer)) //TODO: Nebu didn't know what the Request URI was, so he set it to null.
    val renderArgs = Scope.RenderArgs.current()
     // try to fill context
    for ((sym, value) <- args) {
        context.attributes(sym.name) = value
    }
    context.attributes("playcontext") = PlayContext

    // now add renderArgs as well
    if (renderArgs != null && renderArgs.data != null) {
      for (pair <- renderArgs.data) context.attributes(pair._1) = pair._2
    }
    
    try {
       context.attributes("errors") = validationErrors
    } catch { case ex:Exception => throw new UnexpectedException(ex)}
    
    try {
          val baseName = templateName.replaceAll(".html", "."+renderMode)
          val templatePath = new File(Play.applicationPath+"/app/views","/"+baseName).toString.replace(new File(Play.applicationPath+"/app/views").toString,"")
          engine.layout(templatePath, context)
    } catch {
        case ex:TemplateNotFoundException => {
          if(ex.isSourceAvailable) {
            throw ex

          }
          val element = PlayException.getInterestingStrackTraceElement(ex)
          if (element != null) {
             throw new TemplateNotFoundException(templateName, Play.classes.getApplicationClass(element.getClassName()), element.getLineNumber());
          } else {
             throw ex
          }
        }  
        case ex:InvalidSyntaxException => handleSpecialError(context,ex)
        case ex:CompilerException => handleSpecialError(context,ex)
        case ex:Exception => handleSpecialError(context,ex)
    } finally {throw new ScalateResult(buffer.toString,templateName) }
  }
  
private def handleSpecialError(context:DefaultRenderContext,ex:Exception) {
  context.attributes("javax.servlet.error.exception") = ex
  context.attributes("javax.servlet.error.message") = ex.getMessage
  try {
    engine.layout(engine.load(errorTemplate), context)
  } catch {
    case ex:Exception =>
      // TODO use logging API from Play here...
      println("Caught: " + ex)
      ex.printStackTrace

  }
}
  //determine if we need to render with scalate
private def shouldRenderWithScalate(template:String):Boolean = {
    val ignore = Play.configuration.getProperty("scalate.ignore") 
    if (Play.configuration.containsKey("scalate")) {
      if (ignore != null) {
         ignore.split(",").filter(template.startsWith(_)).size == 0
      } else true
    } else false 
}

private def discardLeadingAt(templateName:String):String = {
        if(templateName.startsWith("@")) {
            if(!templateName.contains(".")) {
                determineURI(controller + "." + templateName.substring(1))
            }
            determineURI(templateName.substring(1))
        } else templateName
  }

private def determineURI(template:String = defaultTemplate):String = {
     template.replace(".", "/") + "." + 
     (if (requestFormat == null)  "html" else requestFormat)
  }


}
