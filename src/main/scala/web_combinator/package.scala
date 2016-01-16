import org.http4s.{Method, Uri, HttpVersion, Headers, Status}

import scodec.bits.ByteVector

import scala.language.higherKinds

import scalaz._, scalaz.Scalaz._
import scalaz.stream._

import web_combinator.DSL._

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

  // Requests

  sealed trait Request[+F[_]]

  case class RequestWithoutBody(
    method: Method,
    uri: Uri,
    httpVersion: HttpVersion,
    headers: Headers)
  extends Request[Nothing]

  type RequestBody[+F[_]] = F[ByteVector]

  case class RequestWithBody[+F[_]]
  (withoutBody: RequestWithoutBody,
   body: RequestBody[F]) extends Request[F]

  def getRequestBody[F[_] : Applicative]
  (request: Request[F]): RequestBody[F] =
    request match {
      case x: RequestWithBody[F] => x.body
      case x: RequestWithoutBody => ByteVector.empty.point[F]
    }

  // Responses

  sealed trait Response[+F[_]]

  case class ResponseWithoutBody(
    status: Status,
    httpVersion: HttpVersion,
    headers: Headers)
  extends Response[Nothing]

  type ResponseBody[+F[_]] = F[ByteVector]

  case class ResponseWithBody[+F[_]]
  (withoutBody: ResponseWithoutBody,
   body: F[ByteVector]) extends Response[F]

  def getResponseBody[F[_] : Applicative]
  (response: Response[F]): ResponseBody[F] =
    response match {
      case x: ResponseWithBody[F] => x.body
      case x: ResponseWithoutBody => ByteVector.empty.point[F]
    }

  // App stuff

  def getResponse[F[_] : Applicative]
  (app: App[F], request: Request[F]): F[Response[F]] =
    app match {
      case emptyApp =>
        (response(status=Status.NotFound): Response[F]).point[F]
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
