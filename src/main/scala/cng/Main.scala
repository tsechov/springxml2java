package cng

import java.io.File
import scala.util.{Failure, Success, Try}
import scala.xml.{NodeSeq, Node, Elem}
import scala.xml.transform.{RuleTransformer, RewriteRule}
import BeanNode._


case class Config(directory: File = new File("."))

object Main extends App {

  val parser = new scopt.OptionParser[Config]("springconfigreplace") {
    head("springconfigreplace", "1.0")
    opt[File]('d', "directory") required() valueName ("<file>") action {
      (x, c) =>
        c.copy(directory = x)
    } text ("directory is a required file property")
  }


  parser.parse(args, Config()).fold({})({
    config => {
      println(s"working in: ${config.directory}")
      FileList.getFileTree(config.directory)
        .filter(properFileName)
        .filter(springXml)
        .flatMap(hasTargetBean)
        .filter(!_._2._2.isEmpty)
        .map(generateCode)
        .map(removeNodes)
        .foreach {
        println _
      }
    }
  })

  def properFileName(file: File) = file.getAbsolutePath().matches("^.*src.main.resources.*\\.xml$")


  def springXml(file: File) = {
    Try(scala.xml.XML.loadFile(file)) match {
      case Success(root) => !(root \\ "bean" isEmpty)
      case Failure(ex) => println(s"ERROR reading: $file\n$ex"); false
    }
  }

  def hasTargetBean(file: File): Map[File, (Elem, NodeSeq)] = {
    //val xmlDefinition=scala.io.Source.fromFile(file).getLines().find( _.startsWith("<?xml"))
    val root = scala.xml.XML.loadFile(file)
    val targetBeans = (root \\ "bean") filter isDomainDataWebProviderDTO
    Map(file ->(root, targetBeans))
  }


  def rewrite = new RewriteRule {
    override def transform(n: Node): NodeSeq = {
      if (n.isDomainDataWebProviderDTO) NodeSeq.Empty
      else n

    }
  }


  def generateCode(in: (File, (Elem, NodeSeq))): (File, Elem) = {
    val xmlPath = in._1.getAbsolutePath.split(File.separatorChar)
    val sourceRoot = xmlPath.takeWhile(_ != "main").mkString(File.separator)
    val moduleNameComponents=xmlPath.takeWhile(_ != "src").last.split('-')
    val moduleName = moduleNameComponents.foldLeft("")((accu: String, elem: String) => accu match {
      case "" => elem
      case "crossng" => elem
      case s: String => accu + elem.capitalize
    })

    val packageName: String = moduleNameComponents :+ "moduleconfig" mkString(".")
    CodeGen.generateConfigClasses(sourceRoot, moduleName, packageName, in._2._2)
    (in._1, in._2._1)
  }

  def removeNodes(in: (File, Elem)): Map[File, NodeSeq] = {
    val root = in._2
    def transformer(x: Node) = new RuleTransformer(rewrite).transform(x)
    val transformed = root flatMap transformer
    Map(in._1 -> transformed)
  }


}
