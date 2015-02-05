package com.timepath.xonotic

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}

class DemoParser {

  implicit def toBoolean(e: Int): Boolean = e != 0

  implicit class DemoStream(input: InputStream) extends CStream(input) {
    def readCoord = readFloat()

    def readAngle = read16

    def readVector = new Vector(readCoord, readCoord, readCoord)

    def readStatic(large: Boolean) = readBaseline(large)

    def readBaseline(large: Boolean) = {
      val (modelIndex, frame) = if (large) (read16, read16) else (read8, read8)
      val colormap = read8
      val skin = read8
      val origin = Vector(0, 0, 0)
      val angles = Vector(0, 0, 0)
      origin.x = readCoord
      angles.x = readAngle
      origin.y = readCoord
      angles.y = readAngle
      origin.z = readCoord
      angles.z = readAngle
    }

    def readStaticSound(large: Boolean) = {
      val org = readVector
      val sound = if (large) read16 else read8
      val vol = read8
      val atten = read8
    }

    def readEntityState() = {
      val frame = read32
      val servermovesequence = read32
      breakable {
        while (true) {
          val n = read16
          if (n == 0x8000) break()
          val e = n & 0x7FFF
          if (n & 0x8000) {
            // remove
          } else {
            var bits = read8
            if (bits & 1 << 7) {
              bits |= read8 << 16
              if (bits & 1 << 15) {
                bits |= read8 << 16
                if (bits & 1 << 23)
                  bits |= read8 << 24
              }
            }
            if (bits & 1 << 8) {
              val flags = read8
            }
            if (bits & 1 << 1) {
              val origin = if (bits & 1 << 11)
                Vector(readFloat(), readFloat(), readFloat())
              else
                Vector(read16, read16, read16)
            }
            if (bits & 1 << 2) {
              val angles = if (bits & 1 << 12)
                Vector(read16, read16, read16)
              else
                Vector(read8, read8, read8)
            }
            if (bits & 1 << 3) {
              val modelindex = if (bits & 1 << 13)
                read16
              else
                read8
            }
            if (bits & 1 << 4) {
              val frame = if (bits & 1 << 21)
                read16
              else
                read8
            }
            if (bits & 1 << 5) {
              val skin = read8
            }
            if (bits & 1 << 6) {
              val effects = if (bits & 1 << 20)
                read32
              else if (bits & 1 << 19)
                read16
              else
                read8
            }
            if (bits & 1 << 9) {
              val alpha = read8
            }
            if (bits & 1 << 10) {
              val scale = read8
            }
            if (bits & 1 << 14) {
              val colormap = read8
            }
            if (bits & 1 << 16) {
              val tagentity = read16
              val tagindex = read8
            }
            if (bits & 1 << 17) {
              val light = (read16, read16, read16, read16)
              val lightstyle = read8
              val lightpflags = read8
            }
            if (bits & 1 << 18) {
              val glowsize = read8
              val glowcolor = read8
            }
            if (bits & 1 << 22) {
              val colormod = (read8, read8, read8)
            }
            if (bits & 1 << 24) {
              val glowmod = (read8, read8, read8)
            }
            if (bits & 1 << 25) {
              val t = read8
              if (t == 0) {
                val frame = (read16, 0, 0, 0)
                val start = (read16, 0, 0, 0)
                val lerp = (1, 0, 0, 0)
              }
              else if (t == 1) {
                val frame = (read16, read16, 0, 0)
                val start = (read16, read16, 0, 0)
                val lerp = (read8, read8, 0, 0)
              }
              else if (t == 2) {
                val frame = (read16, read16, read16, 0)
                val start = (read16, read16, read16, 0)
                val lerp = (read8, read8, read8, 0)
              }
              else if (t == 3) {
                val frame = (read16, read16, read16, read16)
                val start = (read16, read16, read16, read16)
                val lerp = (read8, read8, read8, read8)
              }
              else if (t == 4) {
                val modelindex = read16
                val numbones = read8
                for (bonenum <- 0 until numbones) {
                  val pose7s = (read16, read16, read16, read16, read16, read16, read16)
                }
              }
            }
            if (bits & 1 << 26) {
              val traileffectnum = read16
            }
          }
        }
      }
    }

    def readEntity(in: Int) = {
      var bits = in
      if (bits & U.MOREBITS) bits |= read8 << 8
      if (bits & U.EXTEND1) {
        bits |= read8 << 16
        if (bits & U.EXTEND2) bits |= read8 << 24
      }
      val num = if (bits & U.LONGENTITY)
        read16
      else
        read8
      if (bits & U.MODEL) {
        val modelindex = read8
      }
      if (bits & U.FRAME) {
        val frame = read8
      }
      if (bits & U.COLORMAP) {
        val colormap = read8
      }
      if (bits & U.SKIN) {
        val skin = read8
      }
      if (bits & U.EFFECTS) {
        val effects = read8
      }
      if (bits & U.ORIGIN1) {
        val origin = readCoord
      }
      if (bits & U.ANGLE1) {
        val angle = readAngle
      }
      if (bits & U.ORIGIN2) {
        val origin = readCoord
      }
      if (bits & U.ANGLE2) {
        val angle = readAngle
      }
      if (bits & U.ORIGIN3) {
        val origin = readCoord
      }
      if (bits & U.ANGLE3) {
        val angle = readAngle
      }
      if (bits & U.ALPHA) {
        val alpha = read8
      }
      if (bits & U.SCALE) {
        val scale = read8
      }
      if (bits & U.EFFECTS2) {
        val effects = read8
      }
      if (bits & U.GLOWSIZE) {
        val glowsize = read8
      }
      if (bits & U.GLOWCOLOR) {
        val glowcolor = read8
      }
      if (bits & U.COLORMOD) {
        val color = read8
      }
      if (bits & U.FRAME2) {
        val frame = read8
      }
      if (bits & U.MODEL2) {
        val model = read8
      }
    }
  }

  class Packet(val clientToServer: Boolean, val viewAngles: Vector, val data: Array[Byte]) {
    def parse(): Unit = {
      val b = new CStream(new ByteArrayInputStream(data))
      val cmds = mutable.MutableList[Int]()
      var parsing = true
      while (parsing) breakable {
        val cmd = b.read8
        if (cmd == -1) {
          parsing = false
          break()
        }
        cmds += cmd
        if (cmd & 128) {
          b.readEntity(cmd & 127)
          break()
        }
        val d = cmd match {
          case SVC.NOP =>
            val text = "<-- server to client keepalive"
            s"svc_nop $text"

          case SVC.TIME =>
            val time = b.readFloat
            s"svc_time $time"

          case SVC.CLIENTDATA =>
            var bits = b.readu16
            if (bits & SU.EXTEND1) bits |= b.read8 << 16
            if (bits & SU.EXTEND2) bits |= b.read8 << 24

            if (bits & SU.VIEWHEIGHT) {
              val viewheight = b.read8
            }
            if (bits & SU.IDEALPITCH) {
              val idealPitch = b.read8
            }
            for (i <- 0 to 2) {
              if (bits & SU.PUNCH1 << i) b.read16
              if (bits & SU.PUNCHVEC1 << i) b.readFloat()
              if (bits & SU.VELOCITY1 << i) b.readFloat()
            }
            if (bits & SU.ITEMS) {
              val items = b.read32
            }
            if (bits & SU.VIEWZOOM) {
              val viewZoom = b.readu16
            }
            "svc_clientdata"

          case SVC.VERSION =>
            val version = b.read32
            s"svc_version $version"

          case SVC.DISCONNECT =>
            val text = "Server disconnected"
            s"svc_disconnect $text"

          case SVC.PRINT =>
            val text = b.readString()
            s"svc_print $text"

          case SVC.CENTERPRINT =>
            val text = b.readString()
            s"svc_centerprint $text"

          case SVC.STUFFTEXT =>
            val text = b.readString()
            s"svc_stufftext $text"

          case SVC.DAMAGE =>
            val armor = b.read8
            val blood = b.read8
            val v = b.readVector
            "svc_damage"

          case SVC.SERVERINFO =>
            val i = b.read32
            val maxclients = b.read8
            val gametype = b.read8
            val worldMessage = b.readString()
            breakable {
              while (true) {
                val model = b.readString()
                if (model.isEmpty) break()
              }
            }
            breakable {
              while (true) {
                val sound = b.readString()
                if (sound.isEmpty) break()
              }
            }
            "svc_serverinfo"

          case SVC.SETANGLE =>
            val viewAngles = Vector(b.readAngle, b.readAngle, b.readAngle)
            "svc_setangle"

          case SVC.SETVIEW =>
            val viewEntity = b.read16
            "svc_setview"

          case SVC.LIGHTSTYLE =>
            val i = b.read8
            val map = b.readString()
            "svc_lightstyle"

//          case SVC.SOUND => // TODO

//          case SVC.PRECACHE => // TODO

          case SVC.STOPSOUND =>
            val i = b.read16
            "svc_stopsound"

          case SVC.UPDATENAME =>
            val i = b.read8
            val name = b.readString()
            "svc_updatename"

          case SVC.UPDATEFRAGS =>
            val i = b.read8
            val frags = b.read16
            "svc_updatefrags"

          case SVC.UPDATECOLORS =>
            val i = b.read8
            val colors = b.read8
            "svc_updatecolors"

//          case SVC.PARTICLE => // TODO

//          case SVC.EFFECT => // TODO

//          case SVC.EFFECT2 => // TODO

//          case SVC.SPAWNBASELINE => // TODO

//          case SVC.SPAWNBASELINE2 => // TODO

          case SVC.SPAWNSTATIC =>
            b.readStatic(large = false)
            "svc_spawnstatic"

          case SVC.SPAWNSTATIC2 =>
            b.readStatic(large = true)
            "svc_spawnstatic2"

//          case SVC.TEMP_ENTITY => // TODO

          case SVC.SETPAUSE =>
            val pause = b.readBoolean()
            "svc_setpause"

          case SVC.SIGNONNUM =>
            val i = b.read8
            "svc_signonnum"

          case SVC.KILLEDMONSTER =>
            "svc_killedmonster"

          case SVC.FOUNDSECRET =>
            "svc_foundsecret"

          case SVC.UPDATESTAT =>
            val i = b.read8
            val value = b.read32
            "svc_updatestat"

          case SVC.UPDATESTATUBYTE =>
            val i = b.read8
            val value = b.read8
            "svc_updatestatubyte"

          case SVC.SPAWNSTATICSOUND =>
            b.readStaticSound(large = false)
            "svc_spawnstaticsound"

          case SVC.SPAWNSTATICSOUND2 =>
            b.readStaticSound(large = true)
            "svc_spawnstaticsound2"

          case SVC.CDTRACK =>
            val track = b.read8
            val loop = b.read8
            "svc_cdtrack"

          case SVC.INTERMISSION =>
            "svcermission"

          case SVC.FINALE =>
            val text = b.readString()
            s"svc_finale $text"

          case SVC.CUTSCENE =>
            val text = b.readString()
            s"svc_cutscene $text"

          case SVC.SELLSCREEN =>
            "svc_sellscreen"

          case SVC.HIDELMP =>
            val lmplabel = b.readString()
            "svc_hidelmp"

          case SVC.SHOWLMP =>
            val lmplabel = b.readString()
            val picname = b.readString()
            val (x, y) = (b.read16, b.read16)
            "svc_showlmp"

          case SVC.SKYBOX =>
            val skybox = b.readString()
            s"svc_skybox $skybox"

          case SVC.ENTITIES =>
            b.readEntityState()
            "svc_entities"

//          case SVC.CSQCENTITIES =>

          case SVC.DOWNLOADDATA =>
            val start = b.read32
            val size = b.readu16
            val data = new Array[Byte](size)
            b.readFully(data)
            s"svc_downloaddata $start $size"

          case SVC.TRAILPARTICLES =>
            val entityindex = b.read16
            val effectindex = b.read16
            val start = b.readVector
            val end = b.readVector
            "svc_trailparticles"

          case SVC.POINTPARTICLES =>
            val entityindex = b.read16
            val origin = b.readVector
            val velocity = b.readVector
            val count = b.read16
            "svc_pointparticles"

          case SVC.POINTPARTICLES1 =>
            val effectindex = b.read16
            val origin = b.readVector
            "svc_pointparticles1"

          case unknown =>
            println(s"svc_bad $unknown")
            throw new Exception
        }
        println(d)
      }
    }
  }

  def parse(input: InputStream) {
    val b = new CStream(input)
    val DEMOMSG_CLIENT_TO_SERVER = 0x80000000

    val forcedTrack = b.readLine.toInt
    val l = mutable.MutableList[Packet]()
    while (true) {
      val temp = b.read32
      val size = temp & ~DEMOMSG_CLIENT_TO_SERVER
      val clientToServer: Boolean = (temp & DEMOMSG_CLIENT_TO_SERVER) != 0
      val viewangles = Vector(b.readFloat(), b.readFloat(), b.readFloat())
      val data = new Array[Byte](size)
      b.readFully(data)
      val p = new Packet(clientToServer, viewangles, data)
      l += p
      p.parse()
    }
  }
}

object DemoParser {
  def main(args: Array[String]) {
    val in = new FileInputStream(args(0))
    new DemoParser parse in
  }
}
