import java.io.File
import java.io.StringWriter
import java.util.{HashMap => JHashMap, List => JList}

import htsjdk.samtools.SamReaderFactory
import htsjdk.samtools.ValidationStringency
import htsjdk.samtools.SAMTextHeaderCodec
import htsjdk.samtools.SamReader
import htsjdk.samtools.SAMRecord
import htsjdk.samtools.util.SamRecordIntervalIteratorFactory
import htsjdk.samtools.util.Interval

import scala.collection.JavaConverters._
import Top._

package Bam {

  /*
  ~\.jdks\openjdk-19.0.1\bin\java.exe -jar .\target\scala-2.13\bam.jar .\ENCFF710ELD.bam
 */
  
  class Bam(val bamfile:String){
    final def pullReadsOverCoordinates(intervals: List[Interval], analyze: (SAMRecord) => Boolean) {
      val reader = Bam.open(this.bamfile)
      val iterator = {
        new SamRecordIntervalIteratorFactory()
        .makeSamRecordIntervalIterator(reader, intervals.asJava, true)
      }
      //val read = null.asInstanceOf[SAMRecord]
      var okay = true
      while (iterator.hasNext() && okay) {
        okay = analyze(iterator.next())
      }
      iterator.close()
      reader.close()
    }
  }

  object Bam {
    def parseCoords(e:String):(String, Int, Int) = {
      val Array(chrom, startEnd) = e.split(":").slice(0, 2)
      val Array(start,end)=startEnd.split("-")
      (chrom, start.toInt, end.toInt)
    }
    def intervals(coords:Array[String]):Array[Interval] = {
      coords.map(e => { val (chrom, start, end) = parseCoords(e); new Interval(chrom, start, end)})
    }

    def open(f: String, opts: Option[Map[String, String]] = None): SamReader = {
      SamReaderFactory.makeDefault().validationStringency(ValidationStringency.SILENT).open(new File(f))
    }

    def chromosomeSizes(reader: SamReader): Map[String, Int] = {
      reader.getFileHeader.getSequenceDictionary.getSequences.iterator.asScala.map(x => (x.getSequenceName, x.getSequenceLength)).toMap
    }

    def allocateChromosomeArray(sizes: Map[String, Int], default: Int = 0): JHashMap[String, Array[Int]] = {
      val allocated: Map[String, Array[Int]] = sizes.keys.map(chrom => (chrom, Array.fill(sizes(chrom))(default))).seq.toMap
      val hashed = new JHashMap[String, Array[Int]]
      allocated.foreach(_ match {
        case (k, v) => hashed.put(k, v)
      })
      hashed
    }

    def headStr(f: String): String = {
      val bam = Bam.open(f)
      val header = bam.getFileHeader()
      val str = new StringWriter()
      val codec = new SAMTextHeaderCodec()
      codec.encode(str, header)
      str.toString()
    }




  }
}
