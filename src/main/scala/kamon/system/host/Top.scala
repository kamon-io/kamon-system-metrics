/*
 * =========================================================================================
 * Copyright © 2013-2018 the kamon project <http://kamon.io/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 * =========================================================================================
 */

package kamon.system.host

import java.io.{File, FileFilter, IOException}
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

import kamon.Kamon
import kamon.metric.Histogram
import kamon.system.host.Process.{KernelTime, StartTime, UserTime}
import kamon.system.{CustomMetricBuilder, Metric, MetricBuilder}
import kamon.util.DifferentialSource
import org.slf4j.Logger

import scala.io.Source
import scala.util.Try

/**
 *
 */
object ContextSwitchesMetrics extends MetricBuilder("host.top") with CustomMetricBuilder {
  def build(pid: Long, metricName: String, logger: Logger)  = new Metric {
    val processMetric = ProcessCpuMetrics(metricName)

    val processMetricSource = DifferentialSource(() => contextSwitches)

    override def update(): Unit = {
      val processes = Pid.getAll.map(pid => Process.getProcessByPid(pid)).sorted.take(20)

      processes.flatten.foreach{ process =>
        processMetric.forProcess(process.name).record()


      }

      processMetric.forProcess("").record(processMetricSource.get())
      val (voluntary, nonVoluntary) = contextSwitchesByProcess(pid)
      perProcessVoluntaryMetric.increment(voluntary)
      perProcessNonVoluntaryMetric.increment(nonVoluntary)
      globalMetric.increment(globalContextSwitchSource.get())
    }
  }
}

final case class ProcessCpuMetrics(metricName: String) {
  val ProcessCpuMetric = Kamon.histogram(metricName)

  def forProcess(process: String): Histogram = {
    val processTag = Map("component" -> "system-metrics", "process" -> process)
    ProcessCpuMetric.refine(processTag)
  }
}


object Process {
  type UserTime = Long
  type KernelTime = Long
  type StartTime = Long

  //Hertz (number of clock ticks per second) of your system.
  val Hz: Long = executeCmd("getconf CLK_TCK").map(_.toLong).getOrElse(100L)


  /**
    *
    * @param pid
    * @return
    */
  def getProcessByPid(pid:Int):Option[Process] = {
    firstLineOf(s"/proc/$pid/stat").map { line =>
      val values = line.split(" ")
      //See man proc for how to parse /proc/[pid]/stat
      val name = values(1).replaceFirst("\\(", "").replace(")", "")
      val (uTime, sTime, startTime) = jiffiesByProcess(values)
      Process(pid, name, uTime, sTime, System.currentTimeMillis() - startTime)
    }
  }

  /**
    * A "jiffy" is a unit of CPU time.
    *
    * Exactly what it corresponds to in wall-clock time depends on the architecture and how your kernel is configured,
    * but the important thing is that /proc/stat tells you how many jiffies the CPU has executed in total
    * and /proc/<PID>/stat tells you how many jiffies have been executed by a single process.
    *
    * @return (UserTime, KernelTime, StartTime) in jiffies
    */
  private def jiffiesByProcess(values:Array[String]): (UserTime, KernelTime, StartTime) = {
    val uTime = Option(values(13).toLong).getOrElse(0L) * 1000L / Hz
    val sTime = Option(values(14).toLong).getOrElse(0L) * 1000L / Hz
    val startTime = Option(values(21).toLong).getOrElse(0L) * 1000L / Hz
    (uTime, sTime, startTime)
  }

  /**
    *
    * @param file
    * @return
    */
  private def firstLineOf(file: String): Option[String] = {
    val src = Source.fromFile(file)
    try src.getLines.find(_ => true) finally {
      src.close()
    }
  }

  /**
    *
    * @param cmd
    * @return
    */
  private def executeCmd(cmd:String): Option[String] = {
    import sys.process._
    Try((cmd !!).trim).toOption
  }
}

object Pid {
  val Digits: Pattern = Pattern.compile("\\d+")

  /**
    * Gets an Seq of integers in the /proc directory with only numeric digit
    * filenames, corresponding to processes
    *
    * @return An Seq of integers that represents the process ids.
    */
  def getAll: Seq[Int] = Option(new File("/proc").listFiles(new FileFilter() {
    override def accept(file: File): Boolean =
      Digits.matcher(file.getName).matches()
  }).toList.map(_.getName.toInt)).getOrElse(Nil)
}

/**
  * A process is an instance of a computer program that is being executed.
  *
  * @param pid: process Id.
  * @param name: process name.
  * @param uTime: CPU time spent in user code, measured in clock ticks.
  * @param sTime: CPU time spent in kernel code, measured in clock ticks.
  * @param startTime: Time when the process started, measured in clock ticks.
  */
final case class Process(pid:Int, name:String, uTime:UserTime, sTime:KernelTime, startTime:StartTime) extends Ordered[Process] {
  override def compare(that: Process): Int =
    java.lang.Double.compare((that.uTime + that.sTime) / that.startTime.toDouble, (this.uTime + this.sTime) / this.startTime.toDouble)
}
