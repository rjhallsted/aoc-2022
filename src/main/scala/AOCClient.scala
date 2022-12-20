import zio._
import zio.http._
import zio.http.model.{Headers, Cookie}

object AOCClient {
    type Res[U] = RIO[Client, U]
    
    def getInput(day: Int): Res[String] = {
        val url = s"https://adventofcode.com/2022/day/$day/input"

        val headers = sys.env.get("AOC_SESSION_KEY").map(sessionKey => {
            Headers.cookie(Cookie(name = "session", content = sessionKey, target = Cookie.Type.request))
        })

        for {
            headers <- ZIO.getOrFail(headers)
            res <- Client.request(url, headers = headers)
            body <- res.body.asString
        } yield body
    }
}