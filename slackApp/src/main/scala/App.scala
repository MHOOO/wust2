package woost.slack

import autoconfig.config
import slack.SlackUtil
import slack.models._
import slack.rtm.SlackRtmClient
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global

@config(section = "woost.slack")
object Config {
  val accessToken: String
}

class SlackClient(client: SlackRtmClient)(implicit system: ActorSystem) {
  def start() = {
    val selfId = client.state.self.id
    client.onEvent {
      case e: Message =>
        println(s"Got message from '${e.user}' in channel '${e.channel}': ${e.text}")
        val mentionedIds = SlackUtil.extractMentionedIds(e.text)
        if(mentionedIds.contains(selfId)) {
          client.sendMessage(e.channel, s"<@${e.user}>: Hey!")
        }
      case e => println(s"ignored event: $e")
    }
  }

  def stop() = {
    client.close()
  }
}

object SlackClient {
  def apply(accessToken: String)(implicit system: ActorSystem): SlackClient = {
    val client = SlackRtmClient(accessToken)
    new SlackClient(client)
  }
}

object App extends scala.App {
  implicit val system = ActorSystem("slack")
  val client = SlackClient(Config.accessToken)
  client.start()
}
