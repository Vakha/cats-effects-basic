package catsEffectBasic

import cats.effect.{IO, Resource}
import cats.syntax.all._

import java.io._

/**
 * Simple IO-based program to copy files. First part of cats-effect tutorial
 * at https://typelevel.org/cats-effect/tutorial/tutorial.html
 */
object FileCopying {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f))  // build
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)  // release
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f))
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      // using blocking for input/output operation is preferred
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      count <- if(amount > -1) IO.blocking(destination.write(buffer, 0 , amount)) >>
                               transmit(origin, destination, buffer, acc + amount)
               else IO.pure(acc)
    } yield count

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

  def copyWithBrackets(origin: File, destination: File): IO[Long] = {
    val inIO = IO(new FileInputStream(origin))
    val outIO = IO(new FileOutputStream(destination))

    (inIO, outIO)
      .tupled
      .bracket {
        case (in, out) =>
          transfer(in, out)
      } {
        case (in, out) =>
          (IO(in.close()), IO(out.close()))
            .tupled
            .void
            .handleErrorWith(_ => IO.unit)
      }

  }

}
