package viewer

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

import org.scalajs.dom
import dom.ext.Ajax

import scala.scalajs.js.annotation._

case class Dep(head: String, `type`: String)
case class Word(idx: Int, form: String, lem: String, pos: String, n: String, dep: Option[Dep])
case class Sentence(idx: Int, text: String, ctext: String, id: String, word: Seq[Word], lms: Option[Seq[Metaphor]])
case class Frame(start: Int, end: Int)
case class Metaphor(source: Frame, target: Frame)

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
      def updatePt(sen: Sentence, meta: Metaphor) = pt.update(sen)
      val selector = new MetaphorSelector(".sentences ul", sentences, updatePt)
    }
  }

  /*const data = [];
  data.push({ id: 0, word: 'ROOT', tag: 'ROOT', level: 0 });
  for (let w of mnData.word){
    let depType = '', depHead = 0;
    if(w.dep){
      depType = w.dep.type
      depHead = Number(w.dep.head)
    }
    data.push({id: Number(w.n), word: w.form, dependency: depType, parent: depHead, tag: w.rpos, level: 1});
  }
  return data*/

}