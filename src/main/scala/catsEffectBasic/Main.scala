package catsEffectBasic

import cats.effect._
import java.io.File

object Main extends IOApp {

  def readArgs(args: List[String]): IO[(File, File)] =
    if(args.size < 2)
      IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
    else IO {
      (new File(args(0)), new File(args(1)))
    }

  def runIOCopy(args: List[String]): IO[Unit] =
    for {
      files <- readArgs(args)
      (orig, dest) = files
      count <- FileCopying.copy(orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ()

  def runPolymorphicCopy(args: List[String]): IO[Unit] =
    for {
      files <- readArgs(args)
      (orig, dest) = files
      count <- FileCopyingPolymorphic.copy[IO](orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ()

  // runMain catsEffectBasic.Main src/test/resources/origin.txt src/test/resources/destination.txt
  override def run(args: List[String]): IO[ExitCode] = {
//    runIOCopy(args).map(_ => ExitCode.Success)
    runPolymorphicCopy(args).map(_ => ExitCode.Success)
  }

}
