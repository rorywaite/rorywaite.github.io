package viewer

import scala.collection.mutable
import mutable.Buffer
import mutable.StringBuilder
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html

class MetaphorSelector(listElem: String, data: Seq[Sentence], callback: (Sentence, Metaphor) => Unit) {
  import MetaphorSelector._

  def extractSpans(sen: Sentence) = {
    val spans = for (lms <- sen.lms)
      yield for {
      meta <- lms
    } yield (makeText(sen, Seq(meta), Option(callback)), meta.score)
    spans.getOrElse({
      Seq((Seq(span(`class` := "parseFail")(sen.text)), 0d))
    })
  }

  val metas = for {
    sen <- data
    m <- extractSpans(sen)
  } yield m
  val list = for ((spans, _) <- metas.sortWith(_._2 > _._2)) yield li(spans)
  val elems = dom.document.querySelectorAll(listElem)
  assert(elems.length == 1, s"Found more ${elems.length} metaphor list elems")
  val elem = elems(0).asInstanceOf[html.UList]
  for (li <- list) elem.appendChild(li.render)

}

object MetaphorSelector {
  def makeText(sen: Sentence, lms: Seq[Metaphor], callback: Option[(Sentence, Metaphor) => Unit] = None) = {

    case class Span(start: Int, end: Int, cls: String, meta: Metaphor) {
      def getCls() = if (callback.isEmpty) cls else cls + " clickable"
    }
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
    def makeCB(s: Sentence, m: Metaphor) = callback.map { cb =>
      onclick := { (ev: dom.MouseEvent) => cb(s, m) }
    }
    val htmlSpans = for ((c, i) <- sen.text.zipWithIndex) yield if (starts.contains(i)) {
      val s = clearBuff()
      htmlBuff += c
      s
    } else if (ends.contains(i)) {
      val e = ends(i)
      val s = clearBuff(span(`class` := e.getCls, makeCB(sen, e.meta)))
      htmlBuff += c
      s
    } else {
      htmlBuff += c
      None
    }
    val endBuff = if (ends.contains(sen.text.length)) {
      val e = ends(sen.text.length)
      clearBuff(span(`class` := e.getCls, makeCB(sen, e.meta)))
    } else
      clearBuff()
    (htmlSpans :+ endBuff).flatten
  }
}