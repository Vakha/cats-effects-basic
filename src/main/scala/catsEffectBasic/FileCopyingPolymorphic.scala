package catsEffectBasic

import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.syntax.all._

import java.io._

/**
 * Simple IO-based program to copy files. First part of cats-effect tutorial
 * at https://typelevel.org/cats-effect/tutorial/tutorial.html
 */
object FileCopyingPolymorphic extends IOApp {

  def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].blocking(new FileInputStream(f))
    } { inStream =>
      Sync[F].blocking(inStream.close()).handleErrorWith(_ => Sync[F].unit)
    }

  def outputStream[F[_]: Sync](f: File): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].blocking(new FileOutputStream(f))
    } { outStream =>
      Sync[F].blocking(outStream.close()).handleErrorWith(_ => Sync[F].unit)
    }

  def inputOutputStreams[F[_]: Sync](in: File, out: File): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transmit[F[_]: Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1) Sync[F].blocking(destination.write(buffer, 0, amount)) >>
                                transmit(origin, destination, buffer, amount + acc)
               else Sync[F].pure(acc)
    } yield count

  def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def copy[F[_]: Sync](origin: File, destination: File): F[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

  // runMain catsEffectBasic.FileCopyingPolymorphic src/test/resources/origin.txt src/test/resources/destination.txt
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.raiseWhen(args.length < 2)(new IllegalArgumentException("Need origin and destination files"))
      origFileName = args(0)
      destFileName = args(1)
      _ <- IO.raiseWhen(origFileName == destFileName)(new IllegalArgumentException("Origin and destination files must not be the same"))
      orig = new File(origFileName)
      dest = new File(destFileName)
      count <- copy[IO](orig, dest)
      _     <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success

}
