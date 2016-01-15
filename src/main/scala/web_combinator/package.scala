import org.http4s.{Method, Uri, HttpVersion, Headers, Status}

import scodec.bits.ByteVector

import scala.language.higherKinds

import scalaz._, scalaz.Scalaz._
import scalaz.stream._

package object web_combinator {

  sealed trait App[+F[_]]

  /** An app for which we can generate responses without side-effecting.
    */
  trait PureApp extends App[Id]

  /** A webapp that can represent as a set of files (as served,
    * for example, by a simple web server like apache or nginx).
    */
  trait StaticApp extends PureApp

  /** The most minimal possible webapp definition.
    */
  case object emptyApp extends StaticApp

  def emptyBody[F[_] : Applicative] = ByteVector.empty.point[F]

  case class Request[+F[_]](
    method: Method = Method.GET,
    uri: Uri = Uri(path = "/"),
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty,
    body: F[ByteVector]
  )

  case class Response[+F[_]](
    status: Status = Status.Ok,
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty,
    body: F[ByteVector]
  )

  def getResponse[F[_] : Applicative]
  (app: App[F], request: Request[F]): F[Response[F]] =
    app match {
      case emptyApp =>
        Response(status=Status.NotFound, body=ByteVector.empty.point[F])
          .point[F]
    }

  /** A relative file path.
    *
    * e.g. FilePath(List("foo", "bar.txt")) represents "foo/bar.txt"
    */
  case class FilePath(components: List[String])

  /** Enumerate the files from a static app.
    */
  def getStaticFiles(app: StaticApp): Process0[(FilePath, Process0[ByteVector])] =
    app match {
      case emptyApp => Process.halt
    }
}
