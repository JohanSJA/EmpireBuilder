package models

import play.api.db.slick.Config.driver.simple._

case class Part(name: String, description: String)

class Parts(tag: Tag) extends Table[Part](tag, "PARTS") {
  def name = column[String]("name", O.PrimaryKey)
  def description = column[String]("description")

  def * = (name, description) <> (Part.tupled, Part.unapply _)
}

case class Rate(partName: String, wagesFrom: Double, wagesTo: Double,
                contributionEmployer: Option[Double], contributionEmployee: Option[Double]) {
  def contributionTotal() = {
    val employer = contributionEmployer.getOrElse(0.0)
    val employee = contributionEmployee.getOrElse(0.0)
    val total = employer + employee
    if (total == 0) None
    else Some(total)
  }
}

object Rates {
  def list(partName: String) = {
    val rates10 = Rate(partName, 0.01, 10, None, None)
    partName match {
      case "A" =>
        val rates5k = (20.to(5000, 20)) map { w =>
          val from = if (w == 20) w - 9.99 else w - 19.99
          val employer = (w * 0.13).ceil
          val employee = (w * 0.11).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        val rates20k = (5100.to(20000, 100)) map { w =>
          val from = w - 99.99
          val employer = (w * 0.12).ceil
          val employee = (w * 0.11).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        rates10 :: rates5k.toList ::: rates20k.toList

      case "B" =>
        val employer = 5
        val rates5k = (20.to(5000, 20)) map { w =>
          val from = if (w == 20) w - 9.99 else w - 19.99
          val employee = (w * 0.11).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        val rates20k = (5100.to(20000, 100)) map { w =>
          val from = w - 99.99
          val employee = (w * 0.11).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        rates10 :: rates5k.toList ::: rates20k.toList

      case "C" =>
        val rates5k = (20.to(5000, 20)) map { w =>
          val from = if (w == 20) w - 9.99 else w - 19.99
          val employer = (w * 0.065).ceil
          val employee = (w * 0.055).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        val rates20k = (5100.to(20000, 100)) map { w =>
          val from = w - 99.99
          val employer = (w * 0.060).ceil
          val employee = (w * 0.055).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        rates10 :: rates5k.toList ::: rates20k.toList

      case "D" =>
        val employer = 5
        val rates5k = (20.to(5000, 20)) map { w =>
          val from = if (w == 20) w - 9.99 else w - 19.99
          val employee = (w * 0.055).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        val rates20k = (5100.to(20000, 100)) map { w =>
          val from = w - 99.99
          val employee = (w * 0.055).ceil
          Rate(partName, from, w, Some(employer), Some(employee))
        }
        rates10 :: rates5k.toList ::: rates20k.toList

      case _ => List[Rate]()
    }
  }
}
