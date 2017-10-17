package viewer

import scala.collection.mutable
import mutable.Buffer
import mutable.StringBuilder
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js

class MetaphorSelector(listElem: String, data: Seq[Sentence], callback: (Sentence, Metaphor) => Unit) {

  def extractSpans(id: Int, sen: Sentence) = {
    val spans = for (lms <- sen.lms) yield {
      makeText(id, sen, lms)
    }
    spans.getOrElse({
      Seq(span(`class` := "parseFail")(sen.text))
    })
  }

  def makeText(id: Int, sen: Sentence, lms: Seq[Metaphor]) = {

    case class Span(start: Int, end: Int, cls: String, meta: Metaphor)
    val spans = for (meta <- lms) yield {
      def makeSpan(frame: Frame, cls: String) = {
        Span(frame.start, frame.end, cls, meta)
      }
      Seq(makeSpan(meta.source, "source"), makeSpan(meta.target, "target"))
    }
    val sorted = spans.flatten.sortWith((a, b) => a.start < b.start);
    val unique = sorted.drop(1).foldLeft(Buffer[Span](sorted(0))) { (accum, x) =>
      if (accum.last.start != x.start) {
        accum += x
      }
      accum;
    }
    val starts = Map(unique.map(s => (s.start, s)): _*)
    val ends = Map(unique.map(s => (s.end, s)): _*)

    val htmlBuff = new StringBuilder()
    def clearBuff(makeSpan: TypedTag[html.Span] = span(`class` := "plain")) = {
      val s = if (!htmlBuff.isEmpty)
        Option(makeSpan(htmlBuff.toString))
      else
        None
      htmlBuff.clear()
      s
    }
    val htmlSpans = for ((c, i) <- sen.text.zipWithIndex) yield if (starts.contains(i)) {
      val s = clearBuff()
      htmlBuff += c
      s
    } else if (ends.contains(i)) {
      val e = ends(i)
      val s = clearBuff(span(`class` := e.cls, onclick := { (ev: dom.MouseEvent) => callback(sen, e.meta) }))
      htmlBuff += c
      s
    } else {
      htmlBuff += c
      None
    }
    val endBuff = if (ends.contains(sen.text.length)) {
      val e = ends(sen.text.length)
      clearBuff(span(`class` := e.cls, onclick := { (ev: dom.MouseEvent) => callback(sen, e.meta) }))
    } else
      clearBuff()
    (htmlSpans :+ endBuff).flatten
  }

  val list = data.map(sen => li(extractSpans(-1, sen)))
  val elems = dom.document.querySelectorAll(listElem)
  assert(elems.length == 1, s"Found more ${elems.length} metaphor list elems")
  val elem = elems(0).asInstanceOf[html.UList]
  val disp = elem.style.display
  println(disp)
  //elem.style.height = "1px"
  for (li <- list) elem.appendChild(li.render)

  //elem.style = style
  //list.style.display = "none"
  //dom.window.setTimeout(() => list.style.display = disp, 20)
  //list.style.display = disp
}
