package play.mvc
 
abstract class ScalateController extends ScalaController with scalate.Provider {

  implicit def scalateStringAsTemplateName(name: String) = new {
    def asTemplate(args: Seq[(Symbol, Any)]) = renderWithScalate(name, args)
    def asTemplate = renderWithScalate(name)
  }

  private lazy val not_reached: results.Template = null

  override def Template = {
    renderWithScalate(args = Seq[(Symbol, Any)]())

    not_reached // This line must not been executed.
  }

  override def Template(_args: (Symbol, Any)*) = { 
    renderWithScalate(args = _args) 

    not_reached // This line must not been executed.    
  }
}
