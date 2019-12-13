package kamon.instrumentation.system.process

import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}

import com.sun.jna.Platform
import com.typesafe.config.Config
import kamon.instrumentation.system.process.ProcessMetrics.ProcessInstruments
import kamon.metric.Timer
import kamon.module.{Module, ModuleFactory}
import kamon.tag.TagSet
import kamon.Kamon
import oshi.SystemInfo
import oshi.util.{FileUtil, ParseUtil}

import scala.jdk.CollectionConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class ProcessMetricsCollector(ec: ExecutionContext) extends Module {
  private val _hiccupIntervalPath = "kamon.instrumentation.system.process.hiccup-monitor-interval"
  private val _defaultTags = TagSet.of("component", "process")
  private val _processCpuInstruments = new ProcessInstruments(_defaultTags)
  private val _collectionTask = new MetricsCollectionTask
  private val _collectionSchedule = Kamon.scheduler().scheduleAtFixedRate(scheduleOnModuleEC(_collectionTask), 1, 1, TimeUnit.SECONDS)
  private val _hiccupMonitor = startHiccupMonitor()

  override def stop(): Unit = {
    _hiccupMonitor.terminate()
    _collectionSchedule.cancel(false)
    _collectionTask.cleanup()
  }

  override def reconfigure(newConfig: Config): Unit =
    _hiccupMonitor.updateInterval(newConfig.getDuration(_hiccupIntervalPath))


  private def scheduleOnModuleEC(task: MetricsCollectionTask): Runnable = new Runnable {
    override def run(): Unit =
      task.schedule(ec)
  }

  private def startHiccupMonitor(): HiccupMonitor = {
    val interval = Kamon.config().getDuration(_hiccupIntervalPath)
    val monitorThread = new HiccupMonitor(_processCpuInstruments.hiccups, interval)
    monitorThread.setDaemon(true)
    monitorThread.setName("hiccup-monitor")
    monitorThread.start()
    monitorThread
  }

  private class MetricsCollectionTask {
    private val _systemInfo = new SystemInfo()
    private val _hal = _systemInfo.getHardware()
    private val _os = _systemInfo.getOperatingSystem
    private val _pid = _os.getProcessId()
    private val _processorCount = _hal.getProcessor.getLogicalProcessorCount().toDouble
    private var _previousProcessCpuTime: Array[Long] = Array.empty[Long]

    def schedule(ec: ExecutionContext): Unit = {
      Future {
        recordProcessCpu()
        recordProcessULimits()
      }(ec)
    }

    def cleanup(): Unit = {
      _processCpuInstruments.remove()
    }

    private def recordProcessCpu(): Unit = {
      val process = _os.getProcess(_pid)
      val previous = _previousProcessCpuTime
      val current = Array (
        process.getKernelTime(),
        process.getUserTime(),
        process.getUpTime()
      )

      if(previous.nonEmpty) {
        val kernelTime = math.max(0L, current(0) - previous(0))
        val userTime = math.max(0L, current(1) - previous(1))
        val totalTime = math.max(0L, current(2) - previous(2))
        def toPercent(value: Long): Long = {
          if(totalTime > 0) ((100D * value.toDouble) / totalTime.toDouble / _processorCount).toLong else 0
        }

        _processCpuInstruments.user.record(toPercent(userTime))
        _processCpuInstruments.system.record(toPercent(kernelTime))
        _processCpuInstruments.combined.record(toPercent(userTime + kernelTime))
      }

      _previousProcessCpuTime = current
    }

    private def recordProcessULimits(): Unit = {
      val process = _os.getProcess(_pid)
      _processCpuInstruments.openFilesCurrent.update(Math.max(process.getOpenFiles(), 0))

      Try {
        if (Platform.isLinux()) {
          val allLimits = FileUtil.readFile(String.format(s"/proc/${_pid}/limits"))
          allLimits.asScala.find(_.toLowerCase().startsWith("max open files")).map { openFilesLimitLine =>
            val openFilesLimit = ParseUtil.getNthIntValue(openFilesLimitLine, 1)
            _processCpuInstruments.openFilesLimit.update(openFilesLimit)
          }
        }
      }
    }
  }

  final class HiccupMonitor(hiccupTimeMetric: Timer, duration: Duration) extends Thread {
    @volatile private var _hiccupNanos = duration.toNanos
    @volatile private var _doRun = true

    override def run(): Unit = {
      var shortestObservedDelta = Long.MaxValue

      while (_doRun) {
        val hiccupTime = hic(_hiccupNanos)
        record(hiccupTime, _hiccupNanos)
      }

      def hic(resolution: Long): Long = {
        val start = System.nanoTime
        TimeUnit.NANOSECONDS.sleep(resolution)
        val delta = System.nanoTime() - start
        if (delta < shortestObservedDelta) shortestObservedDelta = delta
        delta - shortestObservedDelta
      }
    }

    /**
      * We'll need fill in missing measurements as delayed
      */
    def record(value: Long, expectedIntervalBetweenValueSamples: Long): Unit = {
      hiccupTimeMetric.record(value)

      if (expectedIntervalBetweenValueSamples > 0) {
        var missingValue = value - expectedIntervalBetweenValueSamples

        while (missingValue >= expectedIntervalBetweenValueSamples) {
          hiccupTimeMetric.record(missingValue)
          missingValue -= expectedIntervalBetweenValueSamples
        }
      }
    }

    def terminate():Unit =
      _doRun = false

    def updateInterval(duration: Duration): Unit =
      _hiccupNanos = duration.toNanos
  }
}

object ProcessMetricsCollector {

  class Factory extends ModuleFactory {
    override def create(settings: ModuleFactory.Settings): Module =
      new ProcessMetricsCollector(settings.executionContext)
  }
}