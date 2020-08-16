package example

import java.time.Instant
import cats.syntax.either._
import io.circe._

object InstantCodec {
  implicit val instantDecoder: Decoder[Instant] =
    Decoder.decodeDouble.emap {
      d => {
        Either.catchNonFatal(Instant.ofEpochMilli((d * 1000 ).toLong))
          .leftMap(d => s"cannot decode from $d")
      }
    }

  implicit val decodeBigDecimal: Encoder[Instant] =
    Encoder.encodeDouble.contramap[Instant](_.toEpochMilli / 1000.0)
}
