package controllers

import play.api._
import play.api.mvc._
import play.api.libs.{ Comet }
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.Play.current

object Application extends Controller {
  def index = WebSocket.using[Either[String, Array[Byte]]] { request => 
      val out = Enumerator.imperative[Either[String, Array[Byte]]] {
    }
    val in = Iteratee.foreach[Either[String, Array[Byte]]] { s =>
      out.push(s)
    }
    (in, out)
  }
}
