import scala.io.{Codec, Source}

object InputReader:
  def loadInput(input: String): List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List(input).mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
