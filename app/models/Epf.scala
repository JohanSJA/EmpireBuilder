package models

case class Part(name: String, description: String)

object Parts {
  def list(): List[Part] = {
    val partADesc = """
    <ol>
      <li>
        The rate of monthly contributions specified in this Part shall apply to:-
        <ol type="a">
          <li>employees who are Malaysian citizens;</li>
          <li>employees who are not Malaysian citizens but are permanent residents of Malaysia; and</li>
          <li>employees who are not Malaysian citizens who have elected to contribute before 1 August 1998.</li>
        </ol>
        until the the employee reached the age of sixty years
      </li>
      <li>
        In this Part:-
        <ol type="a">
          <li>the amount of wages for the month which shall be contributd to the Fund by each employer for each employee shall be according to any limit on the amount of wages and contributions as prescribed by the Board; and</li>
          <li>the amount of contributions for the month for the purpose of subsection 43(3) and section 44A is limited to any limit on the total contributions as prescribed by the Board.</li>
        </ol>
      </li>
    </ol>
    """
    val partA = Part("A", partADesc)
    val partBDesc = """
    <ol>
      <li>
        The rate of monthly contributions specified in this Part shall apply to employees who are not Malaysian citizens:-
        <ol type="a">
          <li>who elect to contribute on or after 1 August 1998;</li>
          <li>who elect to contribute under subsection 54(3) on or after 1 August 1998; and</li>
          <li>who elect to contribute under paragraph 6 of the First Schedule on or after 1 August 2001.</li>
        </ol>
        until the the employee reached the age of sixty years
      </li>
      <li>
        In this Part:-
        <ol type="a">
          <li>the amount of wages for the month which shall be contributd to the Fund by each employer for each employee shall be according to any limit on the amount of wages and contributions as prescribed by the Board; and
          <li>the amount of contributions for the month for the purpose of subsection 43(3) and section 44A is limited to any limit on the total contributions as prescribed by the Board.
        </ol>
      </li>
    </ol>
    """
    val partB = Part("B", partBDesc)
    val partCDesc = """
    <ol>
      <li>
        The rate of monthly contributions specified in this Part shall apply to:-
        <ol type="a">
          <li>employees who are Malaysian citizens;</li>
          <li>employees who are not Malaysian citizens but are permanent residents of Malaysia; and</li>
          <li>employees who are not Malaysian citizens who have elected to contribute before 1 August 1998.</li>
        </ol>
        who have attained the age of sixty years.
      </li>
      <li>
        In this Part:-
        <ol type="a">
          <li>the amount of wages for the month which shall be contributd to the Fund by each employer for each employee shall be according to any limit on the amount of wages and contributions as prescribed by the Board; and
          <li>the amount of contributions for the month for the purpose of subsection 43(3) and section 44A is limited to any limit on the total contributions as prescribed by the Board.
        </ol>
      </li>
    </ol>
    """
    val partC = Part("C", partCDesc)
    val partDDesc = """
    <ol>
      <li>
        The rate of monthly contributions specified in this Part shall apply to employees who are not Malaysian citizens:-
        <ol type="a">
          <li>who elect to contribute on or after 1 August 1998;</li>
          <li>who elect to contribute under subsection 54(3) on or after 1 August 1998; and</li>
          <li>who elect to contribute under paragraph 3 of the First Schedule on or after 1 August 1998; and</li>
          <li>who elect to contribute under paragraph 6 of the First Schedule on or after 1 August 2001.</li>
        </ol>
        who have attained the arge of sixty years.
      </li>
      <li>
        In this Part:-
        <ol type="a">
          <li>the amount of wages for the month which shall be contributd to the Fund by each employer for each employee shall be according to any limit on the amount of wages and contributions as prescribed by the Board; and
          <li>the amount of contributions for the month for the purpose of subsection 43(3) and section 44A is limited to any limit on the total contributions as prescribed by the Board.
        </ol>
      </li>
    </ol>
    """
    val partD = Part("D", partDDesc)
    List(partA, partB, partC, partD)
  }
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
