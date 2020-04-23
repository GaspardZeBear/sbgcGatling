package sbgc

import scala.concurrent.duration._

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.jdbc.Predef._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer


//=================================================================================
object Dumby  {
  var maxindex: String=""
  var count:  String=""
  var size: String=""
  var sleep: String=""
}

//=================================================================================
class Dumby(id: String, loop: Int)  {
//T.jmx:          <stringProp name="HTTPSampler.path">/dumby/${MAXINDEX}/${COUNT}/${SIZE}/${SLEEP}</stringProp>
  val create=repeat(loop,"n") { 
    exec(http(id)
    //  .get("/dumby/8192/1024/512/100")
      .get("/dumby/"+Dumby.maxindex + "/" + Dumby.count + "/" + Dumby.size + "/" + Dumby.sleep)
    )
    }
}

//=================================================================================
object Reset  {
  val reset= 
    exec(http("reset")
      .get("/reset")
    )
}

//=================================================================================
class Target() {
  var url = ""
  var port = ""

  def this(url: String, port: String) {
    this()
    this.url=url;
    this.port=port;
  }

  def getProtocol()  = {
    http.baseUrl(this.url + ":"+this.port)
      .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      .acceptEncodingHeader("gzip, deflate")
      .acceptLanguageHeader("en-US,en;q=0.5")
      .userAgentHeader("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:59.0) Gecko/20100101 Firefox/59.0")
  }
}

//=================================================================================
class Sbgc extends Simulation {

  private def getProperty(propertyName: String, defaultValue: String) = {
    println(s"${propertyName} defaults is ${defaultValue}")
    Option(System.getenv(propertyName))
    .orElse(Option(System.getProperty(propertyName)))
    .getOrElse(defaultValue)
  }

  def loop: Int = getProperty("LOOP","100").toInt
  def url: String = getProperty("URL","http://10.34.199.152")
  def gcsPortsDefaults: String = getProperty("GCSPORTS","PAR:1965,G1G:1966,SHE:1967")
  def gcs: String = getProperty("GCS","PAR,G1G,SHE")
  Dumby.maxindex = getProperty("MAXINDEX","8192")
  Dumby.count = getProperty("COUNT","1024")
  Dumby.size = getProperty("SIZE","512")
  Dumby.sleep = getProperty("SLEEP","100")
  before {
    println(s"For this run :")
    println(s"LOOP=${loop}")
    println(s"URL=${url}")
    println(s"GCS=${gcs}")
    println(s"MAXINDEX=${Dumby.maxindex}")
    println(s"COUNT=${Dumby.count}")
    println(s"SIZE=${Dumby.size}")
    println(s"SLEEP=${Dumby.sleep}")
  }

  val gcPortsDefaultsMap = gcsPortsDefaults
    .split(",")
    .map(_.split(":"))
    .map(x => (x(0),x(1)))
    .toMap  
  val gcPorts = gcs
     .split(",")
     .map(_.split(":"))
     .map(x => (x(0), if ( x.length > 1 ) x(1) else gcPortsDefaultsMap(x(0))) ) 
  val scens = new ListBuffer[io.gatling.core.structure.PopulationBuilder]() 
  for ((gc,port) <- gcPorts) {
    scens += scenario("SbgcSimulation" + gc)
                .exec(Reset.reset,new Dumby(gc,loop).create).inject(atOnceUsers(1))
                .protocols(new Target(url,port).getProtocol())
  }
  setUp(
    scens.toList
  )
}
