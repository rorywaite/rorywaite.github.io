package viewer

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom
import dom.ext.Ajax

import scala.scalajs.js.annotation._

case class Dep(head: String, `type`: String)
case class Word(start: Option[Int], idx: Int, form: String, lem: String, pos: String, n: String, dep: Option[Dep])
case class Sentence(idx: Int, text: String, ctext: String, id: String, word: Seq[Word], lms: Option[Seq[Metaphor]])
case class Frame(start: Int, end: Int, form: String, lemma: String, framenames: Option[Seq[String]], framefamilies: Option[Seq[String]])
case class Metaphor(source: Frame, target: Frame, score: Double)

@JSExportTopLevel("MetaNet")
object MetaNet {

  def loadData() = for (xhr <- Ajax.get("data.json")) yield {
    println(xhr.responseText.length)
    val doc = parse(xhr.responseText).getOrElse(Json.Null)
    doc.hcursor.downField("sentences").as[Seq[Sentence]] match {
      case Left(e) => {
        println(e)
        Seq.empty
      }
      case Right(res) => res
    }
  }

  @JSExport
  def drawMetanet() = {
    println("hello")
    for {
      sentences <- loadData()
    } {
      val pt = new ParseTree(".tree svg")
      val md = new MetaphorDescription(".metaphor")
      def updatePt(sen: Sentence, meta: Metaphor) = {
        pt.update(sen, meta)
        md.update(sen, meta)
      }
      new MetaphorSelector(".sentences ul", sentences, updatePt)
    }
  }

}