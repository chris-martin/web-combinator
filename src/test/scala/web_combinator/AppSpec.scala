package web_combinator

import org.http4s.{Method,Status,Uri}

import scodec.bits.ByteVector

import scalaz._, scalaz.Scalaz._

import org.scalatest._

class AppSpec extends FlatSpec with Matchers {

  val someArbitraryRequests: Seq[Request[Id]] = Seq(
    Request[Id](body=emptyBody[Id]),
    Request[Id](method=Method.POST, uri=Uri(path="/foo"), body=emptyBody[Id]))

  val basic404: Response[Id] =
    Response[Id](status=Status.NotFound, body=ByteVector.empty.point[Id])

  "The empty webapp" should "404" in {
    for (req <- someArbitraryRequests)
      getResponse[Id](emptyApp, req).should(be(basic404))
  }

  it should "have no static files" in {
    getStaticFiles(emptyApp).toSeq.should(be(empty))
  }
}
