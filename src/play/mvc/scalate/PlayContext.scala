package play.mvc.scalate
import play.Play
import scala.collection.JavaConversions._
import play.mvc._


object PlayContext {
   def session =  Scope.Session.current()
   def request = Http.Request.current()
   def flash = Scope.Flash.current()
   def params = Scope.Params.current()
   //tags
   def staticurl(uri:String):String = {
      Router.reverse(Play.getVirtualFile(uri))
   }
   def absoluteurl (action:String):String = {
      request.getBase+url(action)
   }
   def absoluteurl (action:String, map:Any):String = {
      request.getBase+url(action, map)
   }
   def url(uri:String):String = {
     Router.reverse(uri,new java.util.HashMap[String,Object]()).url
   }
   //for some reason scalate can not understand generic types
   private def urlmap(uri:String, map:Any):String = {
     try{
       val im = collection.mutable.Map[String,AnyRef]()++=map.asInstanceOf[Map[String,AnyRef]]
       Router.reverse(uri,im).url
     } catch {
       case ex: ClassCastException => throw new ClassCastException("url tag takes a Map[String,AnyRef] as a second parameter")
     }
   }

   def url(uri:String, params: Any*):String = {

     if(params.length == 1 && params(0).isInstanceOf[Map[_,_]]){
       return urlmap(uri, params(0))
     }

     findActionDefinition(uri, params:_*).url
   }

   def anchor(action: String, params: Any*)
             (body: xml.NodeSeq)
             (implicit attributes: Map[String, String] = Map.empty): xml.NodeSeq = {

     val ad = findActionDefinition(action, params:_*)

     ad.method match {
       case "POST" =>
         val id = play.libs.Codec.UUID
         val href="""javascript:document.getElementById("%s").submit();""".format(id)
         <div>
           <form method='POST'
              id={id}
              style='display:none'
              action={ad.url}></form>{
                <a href={href}>{body}</a> %
                  attributes.foldLeft(xml.Node.NoAttributes){
                     case (attr, (k, v)) => new xml.UnprefixedAttribute(k, v, attr)
             }
           }
         </div>.child

       case "DELETE" =>
         val id = play.libs.Codec.UUID
         val href="""javascript:document.getElementById("%s").submit();""".format(id)
         val action = ad.url + 
                      (if(ad.url.indexOf("?")!= -1) "&" else "?") +
                      "x-http-method-override=DELETE"
         <div>
           <form method='POST'
              id={id}
              style='display:none'
              action={action}></form>{
                <a href={href}>{body}</a> %
                  attributes.foldLeft(xml.Node.NoAttributes){
                     case (attr, (k, v)) => new xml.UnprefixedAttribute(k, v, attr)
             }
           }
         </div>.child

       case "GET" => 
         <a href={ad.url}>{body}</a> %
         attributes.foldLeft(xml.Node.NoAttributes){
           case (attr, (k, v)) => new xml.UnprefixedAttribute(k, v, attr)
         }
     }
   } 

   def form(_action: String, params: Any*) 
           (body: xml.NodeSeq)
           (implicit attributes: Map[String, String] = Map.empty):  xml.NodeSeq = {

     val ad = findActionDefinition(_action, params:_*)

     val enctype = "application/x-www-form-urlencoded"
     var method = ad.method
     var action = ad.url

     if ("GET" != method && "POST" != method){
       action += 
         (if(action.indexOf("?")!= -1) "&" else "?") + 
         "x-http-method-override=%s".format(method)
       method = "POST"
     }

     import play.mvc.Scope.Session 
     val token = Session.current.get.getAuthenticityToken

     <form action={action}
           method={method}
           accept-charset="utf-8"
           enctype={enctype}>{
         body ++ <input type="hidden" name="authenticityToken" value={token} />
       }
     </form> % 
     attributes.foldLeft(xml.Node.NoAttributes){
       case (attr, (k, v)) => new xml.UnprefixedAttribute(k, v, attr)
     }
   }

   private def findActionDefinition(action: String, 
                                    params: Any*): Router.ActionDefinition = {

     def isSimpleParam[X](typ: Class[X]): Boolean = {
       classOf[Number].isAssignableFrom(typ) || 
       typ.equals(classOf[String]) ||
       typ.isPrimitive
     }

     import play.mvc.ActionInvoker
     import java.lang.reflect.Method
     import play.classloading.enhancers.LocalvariablesNamesEnhancer.LocalVariablesNamesTracer
     import play.exceptions.NoRouteFoundException
     import play.mvc.Router
     import play.data.binding.Unbinder

     try{
       val r = new java.util.HashMap[String, java.lang.Object]
       val actionMethod = ActionInvoker.getActionMethod(action)(1).asInstanceOf[Method]
       val names = actionMethod.getDeclaringClass.getDeclaredField("$" + actionMethod.getName + LocalVariablesNamesTracer.computeMethodHash(actionMethod.getParameterTypes)).get(null).asInstanceOf[Array[String]]

       if(names.length < params.length){
         throw new NoRouteFoundException(action, null);
       }

       for{
         (param, i) <- params.zipWithIndex
         val name = if(i < names.length) names(i) else ""}{
         if(param.isInstanceOf[Router.ActionDefinition] && param != null){
           Unbinder.unBind(r, param.toString, name, Array()) //TODO: Nebu just added an empty array here to make things compile.
         } 
         else if (isSimpleParam(actionMethod.getParameterTypes.apply(i))){
           if (param != null) {
             Unbinder.unBind(r, param.toString, name, Array()) //TODO: Nebu just added an empty array here to make things compile.
           }
         } 
         else {
           Unbinder.unBind(r, param, name, Array()) //TODO: Nebu just added an empty array here to make things compile.
         }
       }

       Router.reverse(action, r)
     } 
     catch {
       case ex: ClassCastException => 
         throw new ClassCastException("url tag takes a List[String,AnyRef] as a second parameter")
     }
   }

   implicit def str2xml(x: String) = xml.Text(x)
}

