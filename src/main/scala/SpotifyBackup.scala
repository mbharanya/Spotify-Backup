import java.io.{BufferedWriter, File, FileWriter, IOException}
import java.time.LocalDateTime

import com.wrapper.spotify.SpotifyApi
import com.wrapper.spotify.exceptions.SpotifyWebApiException
import com.wrapper.spotify.model_objects.specification.{Paging, SavedTrack}

import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


object SpotifyBackup extends App {
  val accessToken = "BQCznYs8Jn2bRDsXAEcCJ1ozGGMHHuf_mzrhohu0sHVP0Z7kDvNTvSyAkv-kdjdjFCdTI1-KkxndxEhosB_jAlaLGnD6S30PdmvwacJXa8U5X5U2QnNxEk9qPDg6ttQ75zCfRqzTpYN38stks2Exc-hD4WU5Su_MSKRB"

  implicit val executionCtx = ExecutionContext.global

  val spotifyApi = new SpotifyApi.Builder().setAccessToken(accessToken).build

  for {
    tracks: List[SavedTrack] <- findAll()
    _ = writeToFile(tracks)
  } yield ()


  def findAll(offset: Int = 0, tracks: List[SavedTrack] = Nil): Future[List[SavedTrack]] = {
    val pagingFuture: Future[Paging[SavedTrack]] = toScala(spotifyApi.getUsersSavedTracks.offset(offset).build.executeAsync)
    val offsetTracks: Future[(Int, Int, List[SavedTrack])] = for {
      paging <- pagingFuture
      items = paging.getItems.toList
      newOffset = paging.getOffset + paging.getLimit
      total = paging.getTotal
    } yield (newOffset, total, items)


    offsetTracks.flatMap {
      case (newOffset, total, items)
        if (newOffset <= total) => {
        println(s"page ${offset}/${total}")
        findAll(newOffset, items ++ tracks)
      }
      case _ => Future(tracks)
    }.recoverWith {
      case ioe => {
        ioe.printStackTrace
        throw ioe
      }
    }
  }

  def writeToFile(lines: List[SavedTrack]) = {
    println("Writing file")
    // FileWriter
    val file = new File(LocalDateTime.now().toString)
    val bw = new BufferedWriter(new FileWriter(file))
    lines.map(t =>
      s"${t.getTrack.getArtists.map(_.getName).mkString(",")};${t.getTrack.getAlbum.getName};${t.getTrack.getName}\n"
    ).zipWithIndex.foreach {
      case (l: String, i: Int) => {
        val writeEither = Try(bw.write(l)).toEither
        writeEither match {
          case Left(err) => err.printStackTrace
          case _ => {
            println(s"wrote line ${i + 1}/${lines.length}")
          }
      }
    }
  }

  bw.close()
}

}
