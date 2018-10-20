package com.perhac.experiments.catseffect

import java.io._

import cats.effect._
import cats.implicits._

object CatsEffectTutorial extends IOApp {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.fromAutoCloseable(IO(new FileInputStream(f)))

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.fromAutoCloseable(IO(new FileOutputStream(f)))

  def inputOutputStreams(in: File, out: File): Resource[IO, (FileInputStream, FileOutputStream)] =
    (inputStream(in), outputStream(out)).tupled

  // 3:-O not tail recursive!
  def transmit(source: InputStream, target: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO(source.read(buffer, 0, buffer.length))
      count <- if (amount > -1) IO(target.write(buffer, 0, amount)) *> transmit(source, target, buffer, acc + amount)
      else IO.pure(acc)
    } yield count

  def transfer(source: InputStream, target: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(source, target, buffer, 0L)
    } yield total


  def copy(source: File, target: File): IO[Long] =
    inputOutputStreams(source, target).use((transfer _).tupled)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      filePair <- IO(FilePair(new File(args(0)), new File(args(1))))
        .handleErrorWith(_ => IO.raiseError(new IllegalArgumentException("Need source and target files")))
      count <- copy(filePair.source, filePair.target)
      _ <- IO(println(s"${count}b copied from ${filePair.source.getPath} to ${filePair.target.getPath}"))
    } yield ExitCode.Success

}

case class FilePair(source: File, target: File)
