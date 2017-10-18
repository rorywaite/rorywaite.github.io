package facade

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSGlobal
object tsnejs extends js.Object {

  @js.native
  class tSNE extends js.Object {

    def initDataRaw(data: js.Array[js.Array[Float]]): Unit = js.native

    def step(): Unit = js.native

    def getSolution(): js.Array[js.Array[Float]] = js.native

  }
}