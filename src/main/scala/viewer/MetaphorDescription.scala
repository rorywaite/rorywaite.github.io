package viewer

import scala.collection.mutable
import mutable.Buffer
import mutable.StringBuilder
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html

class MetaphorDescription(tag: String) {

  val elems = dom.document.querySelectorAll(tag)
  assert(elems.length == 1, s"Found ${elems.length} elements")
  val elem = elems(0)

  def update(metaphor: Metaphor) = {
    def frame(f: Frame, cls: String) = {
      val conceptBox = for {
        fnSeq <- f.framenames
        fn <- fnSeq.headOption
      } yield div(`class` := "conceptbox", style := "float: left; padding: 10px")(
        span("Invokes the concept: "), span(`class` := "concept")(fn),
        for (ffSeq <- f.framefamilies; ff <- ffSeq.headOption) yield ul(
          for (f <- ff.split(",")) yield li(f)))

      div(`class` := s"$cls frame", style := "display: inline-block; vertical-align: middle; padding: 10px")(
        p(s"${cls.capitalize} frame"),
        p(style := "text-align: center; font-size: x-large;")(f.form),
        conceptBox)
    }
    val desc = span(
      frame(metaphor.target, "target"),
      div(`class` := "mapping frame", style := "margin: auto; display: inline-block; font-size: xx-large")("↔"),
      frame(metaphor.source, "source"))
    if (elem.childNodes.length != 0)
      elem.removeChild(elem.firstChild)
    elem.appendChild(desc.render)

  }

}
