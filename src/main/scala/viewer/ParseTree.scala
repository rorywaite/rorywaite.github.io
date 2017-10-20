package viewer

import scala.scalajs.js
import js.JSConverters._
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3
import scala.collection.mutable
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom

class ParseTree(svgElement: String) {
  import ParseTree._

  val guidance = div(style := "margin-top: 20px; margin-bottom: 20px;")(
    p(style := "text-align: left;")(
      """Metanet makes its decision based on a linguistic construct called a parse tree, 
    as shown below. Parsers can make mistakes that will cause errors in Metanet. You should check that 
    the metaphor makes sense given this linguistic information. For example, 
    have the source and target words been correctly identified as nouns, verbs, etc? Wikipedia has an """,
      a(href := "https://en.wikipedia.org/wiki/English_grammar")("explanation"), " for these terms."))

  def updateGuidance() = {
    val elem = dom.document.querySelector(svgElement.split(" ")(0))
    println(elem.childElementCount)
    if (elem.childElementCount == 1)
      elem.insertBefore(guidance.render, elem.firstChild)
  }

  val POS_TO_IGNORE = Set("POS", "TO")

  def update(data: Sentence, meta: Metaphor): Unit = {
    import meta._

    val Root = Word(None, 0, "ROOT", "ROOT", "", "1", None)
    val words = Root +: data.word.map { w =>
      val idx = w.idx + 1
      if (POS_TO_IGNORE.contains(w.pos))
        w.copy(idx = idx, pos = "")
      else {
        val dep = w.dep.getOrElse {
          Dep("0", "")
        }
        w.copy(idx = idx, dep = Option(dep))
      }
    }

    val wordWidth = 90;
    val wordHeight = 20;

    val treeWidth = (wordWidth * words.length) - (wordWidth / 3)
    val leftOffset = treeWidth / 40;
    def levelHeight(level: Int) = 2 + (Math.pow(level, 1.8) * 15)
    val spans = for {
      w <- words
      dep <- w.dep
    } yield (w, dep.head.toInt)
    def span(tup: (Word, Int)) = Math.abs(tup._1.idx - tup._2)
    val sorted = spans.sortWith((x, y) => span(x) < span(y))
    val levels = mutable.Map(sorted.map(x => (x._1, span(x))).filter(_._2 == 1): _*)
    def swapSpan(tup: (Word, Int)) = if (tup._1.idx > tup._2) (tup._2, tup._1.idx) else (tup._1.idx, tup._2)
    for (w1 <- sorted.filter(x => span(x) > 1)) {
      val subspanHeights = for {
        (w2, height) <- levels
        (w1start, w2end) = swapSpan(w1)
        if (w2.idx > w1start && w2.idx < w2end)
      } yield height
      val height = if (subspanHeights.isEmpty) 1 else subspanHeights.max + 1
      levels.put(w1._1, height)
    }
    val treeHeight = levelHeight(levels.map(_._2).max) + (2 * wordHeight)

    val svg = d3.select(svgElement);
    // draw svg
    svg.selectAll("text, path").remove();
    svg.attr("width", treeWidth + ((2 * wordWidth) / 3)).attr("height", treeHeight + (wordHeight / 2));

    val activeWords = Map((for {
      w <- words
      start <- w.start
      if (meta.source.start == start || meta.target.start == start)
    } yield if (meta.source.start == start) (meta.source, w)
    else (meta.target, w)): _*)

    def activeClass(w: Word, i: Int) =
      if (w == activeWords(meta.source)) s"w${w.idx} source"
      else if (w == activeWords(meta.target)) s"w${w.idx} target"
      else s"word w${w.idx}"

    svg.selectAll(".word").data(words.toJSArray).enter()
      .append("text")
      .text((d: Word) => d.form)
      .attr("class", (d: Word, i: Int) => "word " + activeClass(d, i))
      .attr("x", (d: Word) => leftOffset + (wordWidth * d.idx))
      .attr("y", treeHeight - wordHeight)

    svg.selectAll(".tag").data(words.toJSArray).enter()
      .append("text")
      .text((d: Word) => posMappings(d.pos))
      .attr("class", (d: Word, i: Int) => s"tag " + activeClass(d, i))
      .attr("x", (d: Word) => leftOffset + (wordWidth * d.idx))
      .attr("y", treeHeight)

    case class Edge(dep: Dep, idx: Int, level: Int) {
      val head = dep.head.toInt
      val bottom = treeHeight - (1.8 * wordHeight)
      val top = bottom - levelHeight(level)
      val left = leftOffset + (idx * wordWidth);
      val right = leftOffset + (dep.head.toInt * wordWidth);
      val mid = (right + left) / 2
      val diff = (right - left) / 4
      val arrow = top + ((bottom - top) * 0.25)
    }

    val edges = for {
      w <- words
      d <- w.dep
    } yield {
      Edge(d, w.idx, levels(w))
    }

    def getPath(start: Word, end: Word) = {
      val startEdge = edges.find(_.idx == start.idx).getOrElse(sys.error(s"Word idx $start does not exist! "))

      def followPath(e: Edge): List[Edge] = if (e.head == end.idx || e.head == 0) e :: Nil
      else {
        val nextEdge = edges.find(_.idx == e.head).getOrElse(sys.error(s"Can't find next ${e.idx} edge! "))
        e :: followPath(nextEdge)
      }

      val path = followPath(startEdge)
      if (path.last.head == 0) None
      else Option(path)
    }

    val activeEdges: Set[Edge] = if (!activeWords.isEmpty) Set() ++ (getPath(activeWords(source), activeWords(target)) ++
      getPath(activeWords(target), activeWords(source))).flatten
    else Set()

    def active(e: Edge) = if (activeEdges.contains(e)) " active" else ""
    println(activeEdges)

    svg.selectAll(".edge").data(edges.toJSArray).enter()
      .append("path")
      .attr("class", (d: Edge) => {
        s"edge w${d.idx} w${d.head}" + active(d)
      })
      .attr("d", (d: Edge) => {
        s"M${d.left},${d.bottom} C${d.mid - d.diff},${d.top} ${d.mid + d.diff},${d.top} ${d.right},${d.bottom}"
      });

    svg.selectAll(".dependency").data(edges.toJSArray).enter()
      .append("text")
      .text((d: Edge) => relationalMappings(d.dep.`type`))
      .attr("class", (d: Edge) => s"dependency w${d.idx} w${d.head}" + active(d))
      .attr("x", (d: Edge) => d.mid)
      .attr("y", (d: Edge) => d.arrow - 7);

    val triangle = d3.svg.symbol[Edge]().`type`("triangle-up").size(5);
    svg.selectAll(".arrow").data(edges.toJSArray).enter()
      .append("path")
      .attr("class", (d: Edge) => s"arrow w${d.idx} w${d.head}" + active(d))
      .attr("d", (d: Edge, i: Int) => {
        triangle(d, i)
      })
      .attr("transform", (d: Edge) => {
        val neg = if (d.idx < d.head) "" else "-"
        s"translate(${d.mid}, ${d.arrow}) rotate(${neg}90)"
      })
    updateGuidance
  }
}

object ParseTree {

  val mod = "modifier"
  val obj = "object"
  val subj = "subject"
  val comp = "clausal"

  def relationalMappings(dep: String) = dep match {
    case "ncmod" => mod
    case "cmod" => mod
    case "xmod" => mod
    case "detmod" => mod
    case "arg_mod" => mod
    case "dobj" => obj
    case "iobj" => obj
    case "obj2" => obj
    case "ncsubj" => subj
    case "xsubj" => subj
    case "csubj" => subj
    case "conj" => "conjunction"
    case "xcomp" => comp
    case "ccomp" => comp
    case "aux" => "auxiliary"
    case "" => ""
    case _ => { println(dep); "depends" }
  }

  val punc = "."
  val pronoun = "pronoun"

  def posMappings(pos: String) = if ("APP$" == pos) pronoun
  else {
    val mapped = for (c <- pos.headOption) yield c match {
      case '!' => pos
      case '"' => pos
      case '(' => pos
      case ')' => pos
      case ',' => pos
      case '-' => pos
      case '.' => pos
      case ':' => pos
      case ';' => pos
      case '?' => pos
      case 'A' => "article"
      case 'C' => "conjunction"
      case 'D' => "determiner"
      case 'I' => "preposition"
      case 'J' => "adjective"
      case 'M' => "number"
      case 'N' => "noun"
      case 'P' => pronoun
      case 'R' => "adverb"
      case 'T' => "to"
      case 'V' => "verb"
      case _ => ""
    }
    mapped.getOrElse("")
  }

}