import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse,QueryStringDecoder, HttpMethod}
import org.jboss.netty.buffer.ChannelBuffer
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future
import scala.util.matching.Regex
import java.net.InetSocketAddress
import util.Properties
import scala.collection.mutable.ListBuffer
import java.util.{List,Map}

object Web {
  def main(args: Array[String]) {
    val router = new Router(
      Seq(
        ("""/users""".r, manifest[UsersHandler]),
        ("""/users/(\d+)""".r, manifest[UsersIdHandler]),
        ("""/users/(\d+)/friends/(\d+)""".r, manifest[UsersIdFriendsHandler])
      )
    )
    val port = Properties.envOrElse("PORT", "8081").toInt
    println("Starting on port:"+port)
    ServerBuilder()
      .codec(Http())
      .name("hello-server")
      .bindTo(new InetSocketAddress(port))
      .build(router)
  }
}

trait RequestHandler {

    def get(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]):Future[HttpResponse] = CommonErrorResponses.notImplemented
    def post(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]):Future[HttpResponse] = CommonErrorResponses.notImplemented
    def put(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]):Future[HttpResponse] = CommonErrorResponses.notImplemented
    def delete(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]):Future[HttpResponse] = CommonErrorResponses.notImplemented

}

class UsersHandler extends RequestHandler {

    override def get(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]) = {
        val response = Response()
        response.setStatusCode(200)
        response.setContentString("/users\n")
        Future(response)
    }

    override def post(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]) = {
        val response = Response()
        response.setStatusCode(200)
        val resp = new String(req.getContent().array())
        response.setContentString(resp+"\n")
        Future(response)
    }

}

class UsersIdHandler extends RequestHandler {
  override def get(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String, List[String]]) = {
        val response = Response()
        response.setStatusCode(200)
        var resp = "/users/" + pathParams(0)
        if(queryParams.get("hi") != null){
            resp += "?hi=" + queryParams.get("hi").get(0)
        }
        resp += "\n"
        response.setContentString(resp)
        Future(response)
  }
}

class UsersIdFriendsHandler extends RequestHandler {
  override def get(req:HttpRequest, pathParams:Seq[String], queryParams:Map[String,List[String]]) = {
        val response = Response()
        response.setStatusCode(200)
        var resp = "/users/" + pathParams(0) + "/friends/" + pathParams(1)
        if(queryParams.get("hi") != null){
            resp += "?hi=" + queryParams.get("hi").get(0)
        }
        resp += "\n"
        response.setContentString(resp)
        Future(response)
  }
}


class Router(h:Seq[(Regex, Manifest[_ <: RequestHandler])]) extends Service[HttpRequest, HttpResponse]{

    val handlers = h

    def apply(req:HttpRequest): Future[HttpResponse] = {

        val qsd = new QueryStringDecoder(req.getUri())
        val pathParams = new ListBuffer[String]

        handlers.find( 
          {
            case (r, _) => {
              val m = r.findFirstMatchIn(qsd.getPath())
              m match {
                  case Some(mtc) =>  mtc.matched == qsd.getPath()
                  case None => false
              }
            }
          }
          ) match {
            case Some((r,hnd)) => {
                val m = r.findFirstMatchIn(qsd.getPath()).get
                for( i <- 1 to m.groupCount){
                    pathParams += m.group(i)
                }
                val h = hnd.erasure.getConstructor().newInstance().asInstanceOf[RequestHandler]
                req.getMethod match {
                  case HttpMethod.GET => h.get(req,pathParams,qsd.getParameters())
                  case HttpMethod.POST => h.post(req,pathParams,qsd.getParameters())
                  case HttpMethod.PUT => h.put(req,pathParams,qsd.getParameters())
                  case HttpMethod.DELETE => h.delete(req,pathParams,qsd.getParameters())
                  case _ => CommonErrorResponses.notImplemented
                }
            }
            case None => {
                val response = Response()
                response.setStatusCode(404)
                Future(response)
            }
        }

    }

}

object CommonErrorResponses {

  def prepare(statusCode:Int,resp:String) = {
    val response = Response()
    response.setStatusCode(statusCode)
    response.setContentString(resp)
    Future(response)
  }
  def badRequest = prepare(400,"")
  def notFound = prepare(404,"")
  def internalError = prepare(500,"")
  def notImplemented = prepare(501,"")

}
