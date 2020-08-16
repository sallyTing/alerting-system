package example

import java.io.{BufferedWriter, File, FileWriter}
import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.traverse._
import cats.syntax.applicativeError._
import cats.instances.list._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import scala.io.Source
import scala.io.StdIn.readLine
import InstantCodec._
import models._

object AlertSystem extends App {

  def read[A](f: Interpreter[IO] => IO[A]) = Kleisli[IO, Interpreter[IO], A](f)

  def program: Kleisli[IO, Interpreter[IO], Seq[Alert]] = for {
    _ <- read(_.log("start the program"))
    config <- read(_.config)
    inputFileName <- read(_.getInput(s"Which file to read from under /${config.fileFolder} folder? (eg. input1.jsonl):"))
    inputName = config.fileFolder + "/" + inputFileName
    content <- read(_.readRateFile(inputName)).onError{
      case e: Throwable => read(_.log(s"Error in reading content from $inputName : ${e.getMessage}"))
    }
    rateList <- content.toList.traverse(line => read(_.parseToRate(line))).onError{
      case e: Throwable => read(_.log(s"Error in parsing content from $inputName : ${e.getMessage}"))
    }
    outputFileName <- read(_.getInput(s"Which file to write to under /${config.fileFolder} folder? (eg. output1.jsonl):"))
    outputName = config.fileFolder + "/" + outputFileName
    result = rateList.foldLeft(AlertResult(Map(), Map(), Seq()))(AlertRuleService.passIndividualRate(config))
    _ <- read(_.writeAlertFile(outputName, result.alerts)).onError{
      case e: Throwable => read(_.log(s"Error in writing alerts [${result.alerts}] to $outputName : ${e.getMessage}"))
    }
    _ <- read(_.log(s"${result.alerts.length} alerts been written into file $outputName"))
  } yield result.alerts

  val interpreter = new Interpreter[IO] {
    val config = IO(Config())
    def log(str: String) = IO(println(str))
    def readRateFile(fileName: String): IO[Seq[String]] = IO {
      val bufferedSource = Source.fromFile(fileName)
      val rates = (for {
        line <- bufferedSource.getLines()
      } yield line).toSeq
      bufferedSource.close
      rates
    }
    def parseToRate(rateString: String): IO[CurrencyConversionRate] =
      IO.fromEither(decode[CurrencyConversionRate](rateString))
    def getInput(question: String): IO[String] = IO(readLine(question))
    def writeAlertFile(fileName: String, alerts: Seq[Alert]): IO[Unit] = IO {
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      alerts.map(a => bw.write(a.asJson.noSpaces + "\r\n"))
      bw.close
    }
  }

  program.run(interpreter).unsafeRunSync()
}