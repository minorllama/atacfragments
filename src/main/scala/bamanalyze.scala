import Bam._
import htsjdk.samtools.SAMRecord
package Main {

  import htsjdk.samtools.SamReader
  import htsjdk.samtools.util.Interval


  class BamAnalyze(val bamfile:String){
    val bam:Bam = new Bam(bamfile)
    final def apply(e:SAMRecord):Boolean = {
      println(e)
      true
    }
  }

  class FragSize(val chrom: String, val start: Int, val end: Int, val cutOffs: Array[Int], val mapQ:Int=10) {
    val N:Int = end - start + 1
    val stratifiedSignal:Array[Array[Int]] = cutOffs.map(i => Array.fill(N)(0))
    val signal:Array[Int]  = Array.fill(N)(0)
    val totalFragSize:Array[Int]  = Array.fill(N)(0)
    val avgFragSize:Array[Double]  = Array.fill(N)(0.0)
    var nReads:Int = 0
    var nReadsAccepted:Int = 0
    val coord = "%s\t%d\t%d".format(chrom, start, end)

    def evalAvgFragSize(): Unit = {
      for(i <- totalFragSize.indices){
        if(signal(i) > 0){
          avgFragSize(i) = totalFragSize(i).toDouble/signal(i).toDouble
        }
      }
    }


    def recordHeader():String = {
      "#chr\tstart[\tend]\tAvgFrag\ttotalFrag\tCov\t" + cutOffs.mkString("\t")
    }

    def record(i:Int):String = {
      val entry = "%f\t%d\t%d\t%s".format(
        avgFragSize(i), totalFragSize(i), signal(i),
        stratifiedSignal.map(x => x(i).toString).mkString("\t"))
      coord + "\t" + entry
    }

    final def signalPerBase(startCoord:Int, endCoord:Int, insertSize:Int): Unit = {
      var index:Int = math.max(startCoord - this.start, 0)
      val stopIndex:Int = endCoord - this.start
      while(index <= stopIndex) { // handle cigar string 
        totalFragSize(index) += insertSize
        signal(index) += 1
        for (i <- cutOffs.indices) {
          if (insertSize <= cutOffs(i)) {
            val stratified = stratifiedSignal(i)
            stratified(index) += 1
          }
        }
        index += 1
      }
    }

    def signalFromFragment(e: SAMRecord):Boolean = {
      val insertSize = e.getInferredInsertSize
      val accepted = {
        !e.getReadUnmappedFlag &&
        !e.getMateUnmappedFlag &&
        e.getProperPairFlag &&
        !e.getDuplicateReadFlag &&
        !e.getNotPrimaryAlignmentFlag &&
        e.getMappingQuality >= mapQ &&
        insertSize > 0
      }
      this.nReads += 1
      if(accepted) {
        this.nReadsAccepted += 1
        signalPerBase(e.getAlignmentStart, e.getAlignmentEnd, e.getInferredInsertSize)
      }
      true
    }
    def apply(bam:Bam): Unit = {
      bam.pullReadsOverCoordinates(List(new Interval(this.chrom, this.start, this.end)), this.signalFromFragment)
      evalAvgFragSize()
    }
    def toArray(): Unit = {
      println(nReads, nReadsAccepted)
      println(recordHeader())
      for(i <- signal.indices){
        println(record(i))
      }
    }
  }

  object BamAnalyze {
    def main(args: Array[String]): Unit = {
      val help =
        """
          |    java ./fragsize.jar -jar -bam="ENCFF710ELD.bam" -coords=chr2:1000000-1050000
          |""".stripMargin
      val cfg = Top.Config(args)
      if(cfg.has("-h")){
        println(cfg.toString())
        println(Top.Metadata.compileTime(cfg.getClass))
      } else {
        val cutOffs = cfg("-cutoffs").split(';').map(_.toInt)
        val bamfile = cfg("-bam")
        val coords = Bam.intervals(cfg("-coords").split(';'))
        coords.foreach(coord => {
          val bam = new Bam(bamfile)
          val regionSignal = new FragSize(coord.getContig, coord.getStart, coord.getEnd, cutOffs)
          regionSignal(bam)
          regionSignal.toArray()
        })
      }
    }

  }
}
