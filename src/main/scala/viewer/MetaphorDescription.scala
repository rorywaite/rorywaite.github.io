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
  val guidance = div(
    p(style := "text-align: left;")(
      "Below is a metaphor detected by Metanet. Two words are highlighted that represent semantic frames. The first is the ",
      span(`class` := "target")("target"), " frame, which anchors the sentence to its current sports context. The second is the ",
      span(`class` := "source")("source"), " frame, which provides a mapping to a non-sports context.  The target frame may or may not ",
      "invoke a concept, however the source frame will always invoke a concept"))

  def update(sen: Sentence, metaphor: Metaphor) = {
    def frame(f: Frame, cls: String) = {
      val conceptBox = for {
        fnSeq <- f.framenames
        fn <- fnSeq.headOption
      } yield div(`class` := "conceptbox", style := "float: left; padding: 10px")(
        span("Invokes the concept: "), span(`class` := "concept")(fn.replace("_", " ")),
        for (ffSeq <- f.framefamilies; ff <- ffSeq.headOption) yield ul(
          for (f <- ff.split(",")) yield li(f)))

      div(`class` := s"$cls frame", style := "display: inline-block; vertical-align: middle; padding: 10px")(
        p(s"${cls.capitalize} frame"),
        p(style := "text-align: center; font-size: x-large;")(f.lemma),
        conceptBox)
    }
    val desc = div(style := "text-align: center; font-size: x-large;")(
      frame(metaphor.target, "target"),
      div(`class` := "mapping frame", style := "margin: auto; display: inline-block; font-size: xx-large")("↔"),
      frame(metaphor.source, "source"))
    while (elem.childNodes.length != 0)
      elem.removeChild(elem.firstChild)
    elem.appendChild(guidance.render)
    elem.appendChild(div(style := "margin: 30px; text-align: center; font-size: xx-large;")(MetaphorSelector.makeText(sen, Seq(metaphor))).render)
    elem.appendChild(desc.render)

  }

}
