package viewer

import scala.scalajs.js
import js.JSConverters._
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3
import scala.collection.mutable
import mutable.Buffer

class ParseTree(svgElement: String) {
  val POS_TO_IGNORE = Set("POS", "TO")

  def update(data: Sentence) = {
    val Root = Word(0, "ROOT", "ROOT", "", "1", None)
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

    val wordWidth = 70;
    val wordHeight = 20;

    val wordMap = Map(words.map(w => (w.idx, w)): _*)

    val treeWidth = (wordWidth * words.length) - (wordWidth / 3)
    val leftOffset = treeWidth / 40;
    def levelHeight(level: Int) = 2 + (Math.pow(level, 1.8) * 10)
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
        if (w2.idx >= w1start && w2.idx <= w2end)
      } yield height
      val height = if (subspanHeights.isEmpty) 1 else subspanHeights.max + 1
      levels.put(w1._1, height)
    }
    val treeHeight = levelHeight(levels.map(_._2).max) + (2 * wordHeight)

    val svg = d3.select(svgElement);
    // draw svg
    svg.selectAll("text, path").remove();
    svg.attr("width", treeWidth + ((2 * wordWidth) / 3)).attr("height", treeHeight + (wordHeight / 2));

    val wordTags = svg.selectAll(".word").data(words.toJSArray).enter()
      .append("text")
      .text((d: Word) => d.form)
      .attr("class", (d: Word) => s"word w${d.idx}")
      .attr("x", (d: Word) => leftOffset + (wordWidth * d.idx))
      .attr("y", treeHeight - wordHeight)
      .on("mouseover", (d: Word, _, _) => {
        svg.selectAll(".word, .dependency, .edge, .arrow").classed("active", false);
        svg.selectAll(".tag").attr("opacity", 0);
        svg.selectAll(s".w${d.idx}").classed("active", true);
        svg.select(s".tag.w${d.idx}").attr("opacity", 1);
        Unit
      }).on("mouseout", (d: Word, _, _) => {
        svg.selectAll(".word, .dependency, .edge, .arrow").classed("active", false);
        svg.selectAll(".tag").attr("opacity", 0);
        Unit
      });

    val tags = svg.selectAll(".tag").data(words.toJSArray).enter()
      .append("text")
      .text((d: Word) => d.pos)
      .attr("class", (d: Word) => s"tag w${d.idx}")
      .attr("x", (d: Word) => leftOffset + (wordWidth * d.idx))
      .attr("y", treeHeight)
      .attr("opacity", 0);

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

    val edgeTags = svg.selectAll(".edge").data(edges.toJSArray).enter()
      .append("path")
      .attr("class", (d: Edge) => {
        s"edge w${d.idx} w${d.head}"
      })
      .attr("d", (d: Edge) => {
        s"M${d.left},${d.bottom} C${d.mid - d.diff},${d.top} ${d.mid + d.diff},${d.top} ${d.right},${d.bottom}"
      });

    val dependencies = svg.selectAll(".dependency").data(edges.toJSArray).enter()
      .append("text")
      .text((d: Edge) => d.dep.`type`)
      .attr("class", (d: Edge) => s"dependency w${d.idx} w${d.head}")
      .attr("x", (d: Edge) => d.mid)
      .attr("y", (d: Edge) => d.arrow - 7);

    val triangle = d3.svg.symbol[Edge]().`type`("triangle-up").size(5);
    val arrows = svg.selectAll(".arrow").data(edges.toJSArray).enter()
      .append("path")
      .attr("class", (d: Edge) => s"arrow w${d.idx} w${d.head}")
      .attr("d", (d: Edge, i: Int) => {
        triangle(d, i)
      })
      .attr("transform", (d: Edge) => {
        val neg = if (d.idx < d.head) "" else "-"
        s"translate(${d.mid}, ${d.arrow}) rotate(${neg}90)"
      })
  }
}
