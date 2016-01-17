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

  // Body
  type Body1 = ByteVector
  type BodyF[+F[_]] = F[Body1]
  val emptyBody = ByteVector.empty

  // Message
  case class MessageF[+A, +F[_]](a: A, body: BodyF[F])
  type Message1[+A] = MessageF[A, Id]

  // Request
  case class RequestPrelude(
    method: Method, uri: Uri, httpVersion: HttpVersion, headers: Headers)
  type Request1 = MessageF[RequestPrelude, Id]
  type RequestF[+F[_]] = MessageF[RequestPrelude, F]

  // Response
  case class ResponsePrelude(
    status: Status, httpVersion: HttpVersion, headers: Headers)
  type Response1 = MessageF[ResponsePrelude, Id]
  type ResponseF[+F[_]] = MessageF[ResponsePrelude, F]

  def getResponse[F[_] : Applicative]
  (app: App[F], request: RequestF[F]): F[ResponseF[F]] =
    app match {
      case emptyApp =>
        withEmptyBody[ResponsePrelude, F](response(status=Status.NotFound)).point[F]
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
