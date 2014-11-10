package controllers

import java.util.NoSuchElementException
import play.api._
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.mvc._

import models._

object Epf extends Controller {

  val parts = TableQuery[Parts]
  val rates = TableQuery[Rates]

  def index = DBAction { implicit rs =>
    Ok(views.html.epf(parts.list))
  }

  def part(partName: String) = DBAction { implicit rs =>
    try {
      val partInfo = parts.filter(_.name === partName).first
      val rates10 = Rate(partName, 10, None, None)
      val rates5k = (20.to(5000, 20)) map { w =>
        val employer = (w * 0.13).ceil
        val employee = (w * 0.11).ceil
        Rate(partName, w, Some(employer), Some(employee))
      }
      val rates20k = (5100.to(20000, 100)) map { w =>
        val employer = (w * 0.12).ceil
        val employee = (w * 0.11).ceil
        Rate(partName, w, Some(employer), Some(employee))
      }
      val partRates = rates10 :: rates5k.toList ::: rates20k.toList
      Ok(views.html.epfPart(partInfo, partRates))
    } catch {
      case e: NoSuchElementException =>
        Ok("Unknown part")
    }
  }

  def rate(part: String, wages: Double) = DBAction { implicit rs =>
    try {
      val rateInfo = rates.filter(_.partName === part).filter(_.wagesTo >= wages).sortBy(_.wagesTo.asc).first
      println(rateInfo)
      Ok(views.html.epfRate(wages, rateInfo))
    } catch {
      case e: NoSuchElementException =>
        Ok(s"Unknown rate for $wages")
    }
  }

}
