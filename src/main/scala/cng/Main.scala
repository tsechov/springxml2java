package cng

import java.io.File
import scala.util.{Failure, Success, Try}
import scala.xml.{NodeSeq, Node, Elem}


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
        .filter(_._2._2.isEmpty)
        .foreach(println)
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
    val root = scala.xml.XML.loadFile(file)
    val targetBeans = (root \\ "bean") filter isDomainDataWebProviderDTO
    Map(file ->(root , targetBeans))
  }

  def isDomainDataWebProviderDTO(n:Node) = (n \ "@class").text.endsWith("DomainDataWebProviderDTO")
}
