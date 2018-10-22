package coop.rchain.casper

import java.io.{BufferedReader, FileReader}
import java.nio.file.Path

import cats.Applicative
import cats.effect.{Resource, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Signature
import coop.rchain.casper.util.SignatureAlgorithms
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.{Log, LogSource}

case class ValidatorIdentity(
    publicKey: Array[Byte],
    privateKey: Array[Byte],
    sigAlgorithm: String
) {
  def signature(data: Array[Byte]): Signature = {
    val sig = SignatureAlgorithms.lookup(sigAlgorithm)(data, privateKey)
    Signature(ByteString.copyFrom(publicKey), sigAlgorithm, ByteString.copyFrom(sig))
  }
}

object ValidatorIdentity {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def fileContent[F[_]: Sync](path: Path): F[String] = {
    val openFile = Sync[F].delay(new BufferedReader(new FileReader(path.toFile)))
    Resource.fromAutoCloseable(openFile).use(br => Sync[F].delay(br.readLine()))
  }

  def fromConfig[F[_]: Applicative: Log: Sync](conf: CasperConf): F[Option[ValidatorIdentity]] =
    conf.privateKeyPath match {
      case Some(privateKeyPath) =>
        for {
          fileContent <- fileContent[F](privateKeyPath)
          privateKey  = Base16.decode(fileContent)
          _           = println(s"PRIVATE!!!: $fileContent")
          publicKey   = CasperConf.publicKey(conf.publicKey, conf.sigAlgorithm, privateKey)
        } yield ValidatorIdentity(publicKey, privateKey, conf.sigAlgorithm).some

      case None =>
        Log[F]
          .warn("No private key detected, cannot create validator identification.")
          .map(_ => none[ValidatorIdentity])
    }
}
