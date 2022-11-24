import java.util.jar._
import java.net.JarURLConnection
import java.util.Date

import java.io._
import java.nio.file.{Files, Paths}
import java.util.zip._
import scala.collection.mutable.ArrayBuffer
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL.WithDouble._
import org.json4s.native.Serialization

package Top {

  import scala.jdk.CollectionConverters._

  object Utils {
      def strTrace(e: Throwable) = {
        val sw = new StringWriter();
        val pw = new PrintWriter(sw);
        e.printStackTrace(pw)
        sw.toString()
      }
      def timestamp(): String = "\\s|:".r.replaceAllIn((new Date(System.currentTimeMillis())).toString, ".")
    }

    object IO {
      def readAllBytes(f: String): Array[Byte] = Files.readAllBytes(Paths.get(f))

      def toFile(f: String, obj: Object): String = {
        val file = new FileOutputStream(f)
        val buffer = new BufferedOutputStream(file)
        val serializeObj = new ObjectOutputStream(buffer)
        serializeObj.writeObject(obj)
        serializeObj.close()
        buffer.close()
        file.close()
        f
      }

      def fromFile[T](f: String): T = {
        /* IO.toFile(f, test)
         IO.fromFile[Array[Int]](f)*/
        val file = new FileInputStream(f)
        val buffer = new BufferedInputStream(file)
        val deserializeObj = new ObjectInputStream(buffer)
        val readObject = deserializeObj.readObject()
        readObject.asInstanceOf[T]
      }

      def toBytes(obj: Object): Array[Byte] = {
        val serializer = new ByteArrayOutputStream()
        val serializeObj = new ObjectOutputStream(serializer)
        serializeObj.writeObject(obj)
        serializeObj.flush()
        serializer.flush()
        serializer.toByteArray
      }

      def fromBytes[T](bytes: Array[Byte]): T = {
        val deserializer = new ByteArrayInputStream(bytes)
        val deserializeObj = new ObjectInputStream(deserializer)
        deserializeObj.readObject().asInstanceOf[T]
      }

      def gzip(bytes: Array[Byte]): Array[Byte] = {
        val byteStream = new ByteArrayOutputStream(bytes.length)
        val gzipStream = new GZIPOutputStream(byteStream)
        gzipStream.write(bytes)
        gzipStream.close()
        byteStream.close()
        byteStream.toByteArray
      }

      def gzip(f: String, bytes: Array[Byte]): String = {
        val file = new FileOutputStream(f)
        val buffer = new BufferedOutputStream(file)
        val gzipStream = new GZIPOutputStream(buffer)
        gzipStream.write(bytes)
        gzipStream.close()
        buffer.close()
        file.close()
        f
      }

      def gunzip(bytes: Array[Byte]): Array[Byte] = {
        val byteStream = new ByteArrayInputStream(bytes)
        val gunzipStream = new GZIPInputStream(byteStream)
        val outputStream = new ByteArrayOutputStream()
        val buffer = new Array[Byte](1024)
        var nBytes = gunzipStream.read(buffer)
        while (nBytes > 0) {
          outputStream.write(buffer, 0, nBytes)
          nBytes = gunzipStream.read(buffer)
        }
        gunzipStream.close()
        byteStream.close()
        outputStream.flush()
        outputStream.toByteArray
      }

      def mkdirs(path: String): Boolean = (new File(path)).mkdirs()

      def exists(f: String): Boolean = new File(f).exists

      def write(target: String, data: String) {
        val writer = new BufferedWriter(new FileWriter(target))
        writer.write(data)
        writer.close()
      }

      def writeLines(target: String, data: Array[String]) {
        val writer = new BufferedWriter(new FileWriter(target))
        data.foreach(e => {
          writer.write(e)
          writer.write("\n")
        })
        writer.close()
      }

      def readLines(source: String): Array[String] = {
        var (line, infile) = ("", new BufferedReader(new FileReader(source)))
        val data = ArrayBuffer[String]()
        while ( {
          line = infile.readLine; line != null
        }) {
          data += line
        }
        infile.close()
        data.toArray
      }
      def ftext(f:String,encoding:String="utf-8"): String = {
        readLines(f).mkString("\n")
      }
    }

    class Config(val flags: Array[String] = Array(), val keys: Map[String, String] = Map(), val values: Array[String] = Array()) {
      lazy val flagsSet = Set[String]() ++ flags
      val valsFlag = "-vals"
      var additionalConfig = Map[String, List[String]]()

      def config = new Config(flags, keys, values)

      def has(x: String) = flagsSet(x) || keys.contains(x)

      def array():Array[String] =  {
        if (keys.contains(this.valsFlag)) {
          IO.readLines(keys(this.valsFlag))
        } else if(flagsSet.contains(this.valsFlag)){
          val reader = new BufferedReader(new InputStreamReader(System.in))
          var line = ""
          val data = ArrayBuffer[String]()
          try while ({ line = reader.readLine; line != null }) {
            data += line
          } finally {
            //reader.close
          }
          data.toArray
        } else {
          this.values
        }
      }
      def apply(k: String) = keys(k)
      def get(k: String) = keys.get(k)
      def getOrElse(k: String, default: String):String = {
        keys.getOrElse(k, default)
      }
      override def toString():String = {   Json(Map("keys" -> keys, "flags" -> flagsSet, "values" -> values)) }
    }

    object Config {
      def errlog = System.err.println

      val DELIMITS = Map("SPACE" -> " ", "TAB" -> "\t")
      val FLAGMARK = "-"
      val altDelimiterFlag = "-DELIMIT"
      def config(flags: Array[String] = Array(), keys: Map[String, String] = Map(), values: Array[String] = Array()) = {
        new Config(flags, keys, values)
      }

      // commandline delimiter codes
      def delimitCodes(arg: String) = DELIMITS.getOrElse(arg, arg)

      def apply(): Config = new Config

      def apply(args: Array[String], delimit: String = "="): Config = {
        if (args.length == 0) {
          new Config()
        } else if (args.head.startsWith(altDelimiterFlag)) {
          apply(args.tail, args.head.replace(altDelimiterFlag, ""))
        } else {
          val (flags_keys, values) = args.partition(_.slice(0,1).equals(FLAGMARK))
          val (flagsArray, keyArgs) = flags_keys.map(_.split(delimit)).partition(_.length == 1)
          val flags = flagsArray.map(_.head)
          val keysValListHash = keyArgs.map(k => k match {
              case Array(a, b) => (a, b)
              case _ => throw new IllegalArgumentException("ambiguous:[%s]".format(k.mkString(" ")))
            }).groupBy(_._1)
          val keys = for ((k, v) <- keysValListHash) yield {
            if (v.length == 1) {
              (k, v.head._2)
            } else {
              throw new IllegalArgumentException((k, v.mkString(" ")).toString)
            }
          }
          new Config(flags, keys, values)
        }
      }
    }

    object Collections {
      def pass(): Unit = {
      }
    }


    object Metadata {
      def compileTimeSeconds(cl: Class[_]):Long = {
        val resourceName: String = cl.getName().replace('.', '/') + ".class"
        val jarConx: JarURLConnection = ClassLoader.getSystemResource(resourceName).openConnection.asInstanceOf[JarURLConnection]
        jarConx.getJarFile().getEntry("META-INF/MANIFEST.MF").getTime()
      }
      def compileTime(cl: Class[_]):Map[String,(String, Long)] = {
        try {
          val milliseconds = compileTimeSeconds(cl)
          val time = new java.util.Date(milliseconds)
          Map(cl.getName -> (time.toString, milliseconds))
        } catch {
          case e: Throwable => Map(cl.getName -> (Utils.strTrace(e), -1))
        }
      }

      object JarMainClass {
        import java.util.jar._
        import scala.collection.JavaConverters._
        def apply(jar: String) = {
          val jarfile = new JarFile(new File(jar))
          val jarAttributes: java.util.jar.Attributes = jarfile.getManifest.getMainAttributes
          //jarAttributes.asScala.map((x, y) => Config.errlog(x.toString, y.toString))
          jarAttributes
        }
      }
    }


  object Json {

    /*def xmlJson(xmlFile: String): String = {
      import net.liftweb.json._
      val xml = scala.xml.XML.loadFile(source)
      val jsonObj = Xml.toJson(xml)
      Printer.pretty(render(jsonObj))
    }*/

    abstract class Jsonizable {
      def toString(): String
    }

    class MarshallMap(val m: Map[String, Any], val prettyPrinting: Boolean = false) extends Jsonizable {
      override def toString() = if (prettyPrinting) pretty(m) else apply(m)
    }

    class MarshallList(val l: List[Any], val prettyPrinting: Boolean = false) extends Jsonizable {
      override def toString() = if (prettyPrinting) pretty(l) else apply(l)
    }

    object Marshall {
      def apply(l: List[Any]) = new MarshallList(l)

      def apply(m: Map[String, Any]) = new MarshallMap(m)
    }

    implicit val formats = org.json4s.DefaultFormats

    /*def __pretty(m:Map[String, Any]) = {
      pretty(render(m))
    }*/
    def pretty(m: String) =  Serialization.writePretty(m)

    def pretty(m: Map[String, Any]) = Serialization.writePretty(m)

    def pretty(l: List[Any]) = Serialization.writePretty(l)

    def apply(m: String) = Serialization.write(m)

    def apply(m: Map[String, Any]):String = Serialization.write(m)

    def apply(l: List[Any]) = Serialization.write(l)

    def loads(json: String) = parse(json)

    def dumpf(target: String, obj: Jsonizable) = {
      IO.write(target, obj.toString)
      target
    }

    // NO IMPLICITS!
    def dumpf(target: String, m: Map[String, Any]): String = dumpf(target, new MarshallMap(m, prettyPrinting = true))

    def dumpf(target: String, l: List[Any]): String = dumpf(target, new MarshallList(l, prettyPrinting = true))

    def loadf(target: String, encoding: String = "utf-8") = {
      val text = IO.ftext(target, encoding=encoding)
      loads(text)
    }

    def loadMap(target: String, isFile: Boolean, encoding: String = "utf-8") = {
      val loaded = if (isFile) { loadf(target, encoding) } else { loads(target) }
      loaded.extract[Map[String, Any]]
    }
  }
  }