package retcalc

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int,
                         netIncome: Int,
                         currentExpenses: Int,
                         initialCapital: Double)

object RetCalc {
  def simulatePlan(returns: Returns,
                   params: RetCalcParams,
                   nbOfMonthsSaving: Int) : (Double, Double) = {
    import params._
    val capitalAtRetirement = futureCapital(
      returns = returns,
      nbOfMonths = nbOfMonthsSaving,
      initialCapital = initialCapital,
      currentExpenses = currentExpenses,
      netIncome = netIncome)

    val capitalAfterDeath = futureCapital(
      returns = OffsetReturns(returns, nbOfMonthsSaving),
      nbOfMonths = nbOfMonthsInRetirement,
      initialCapital = capitalAtRetirement,
      currentExpenses = currentExpenses,
      netIncome = 0)

    (capitalAtRetirement,capitalAfterDeath)
  }


  def nbOfMonthsSaving(returns: Returns,
                       params: RetCalcParams) : Int = {
    import params._
    @tailrec
    def loop(months: Int): Int = {
     val capitalAfterDeath = simulatePlan(returns,
       params, months)._2

      if (capitalAfterDeath > 0.0)
        months

      else
        loop(months + 1)
    }
    if (netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }


  def futureCapital(returns: Returns, nbOfMonths: Int, netIncome: Int, currentExpenses: Int,
                    initialCapital: Double): Double = {
    val monthlySavings = netIncome - currentExpenses
    (0 until nbOfMonths).foldLeft(initialCapital) {
      case (accumulated, month) =>
        accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }
}

