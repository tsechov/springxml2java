package cng

import scala.xml.{Node, NodeSeq}
import com.sun.codemodel.{JExpr, JDefinedClass, JMod, JCodeModel}
import BeanNode._
import java.io.File


object CodeGen {
  def generateConfigClasses(sourceRoot: String, moduleName: String, packageName: String, beans: NodeSeq): Unit = {
    val codeModel = new JCodeModel();

    val classNamePrefix = moduleName.capitalize
    val definedClass = codeModel._class(s"${packageName}.${classNamePrefix}Config")
    definedClass.annotate(codeModel.directClass("org.springframework.context.annotation.Configuration"))



    val result = beans.foldLeft((codeModel, definedClass))(generateConfigBean)


    result._1.build(new File(sourceRoot + File.separator + "main" + File.separator + "java"))
  }

  def generateConfigBean(in: (JCodeModel, JDefinedClass), n: Node): (JCodeModel, JDefinedClass) = {
    println(n)
    val key = n.key
    val relativeUrl = n.relativeUrl
    val dtoClass = in._1.directClass(n.className)

    val dtoMethod = in._2.method(JMod.PRIVATE, dtoClass, "webProviderDto"+(key.split('.').last).filterNot( _ == '_'))
    dtoMethod.annotate(in._1.directClass("org.springframework.context.annotation.Bean"))


    val configKey=JExpr.direct(key)
    val url=JExpr.direct(relativeUrl)
    dtoMethod.body()._return(JExpr._new(dtoClass).arg(configKey).arg(url))
    in


  }
}
