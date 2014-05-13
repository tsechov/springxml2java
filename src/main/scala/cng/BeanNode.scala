package cng

import scala.xml.Node


case class BeanNode(n: Node) {
  def className = (n \ "@class").text

  def isDomainDataWebProviderDTO = className.endsWith("DomainDataWebProviderDTO")

  def key = attribute("key")



  def relativeUrl = attribute("relativeURL")

  private def attribute(name: String) = {
    n.attribute("http://www.springframework.org/schema/c", name) match {
      case Some(attributes) => attributes.head.text.stripPrefix("#{T(").stripSuffix("}").filterNot(_ == ')')
      case _ => throw new IllegalArgumentException(s"there is no [http://www.springframework.org/schema/c:$name] attribute on $n")
    }
  }
}

object BeanNode {
  implicit def node2BeanNode(n: Node): BeanNode = BeanNode(n)

  def isDomainDataWebProviderDTO(n: Node) = BeanNode(n).isDomainDataWebProviderDTO
}

