package retcalc

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}

class RetCalcSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "RetCalc.futureCapital" should {
    "calculate the amount of savings I will have in n months" in {
      val actual = RetCalc.futureCapital(
        returns = FixedReturns(0.04), nbOfMonths = 25 * 12,
        netIncome = 3000, currentExpenses = 2000,
        initialCapital = 10000)
      val expected = 541267.1990
      actual should ===(expected)
    }
    "calculate how much savings will be left after having taken a  pension for n months" in {
      val actual = RetCalc.futureCapital(
        FixedReturns(0.04), nbOfMonths = 40 * 12,
        netIncome = 0, currentExpenses = 2000, initialCapital = 541267.1990)
      val expected = 309867.53176
      actual should ===(expected)
    }
  }
    val params = RetCalcParams(
      nbOfMonthsInRetirement = 40 *12,
      netIncome = 3000,
      currentExpenses = 2000,
      initialCapital = 10000)

    "RetCalc.simulatePlan" should {
      "calculate the capital at retirement and the capital after death" in {
        val (capitalAtRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
          returns = FixedReturns(0.04),
          params,
          nbOfMonthsSaving = 25 * 12)
        capitalAtRetirement should === (541267.1990)
        capitalAfterDeath should === (309867.5316)
      }
      "use different returns for capitalisation and drawdown" in {
        val nbOfMonthsSavings = 25 * 12
        val returns = VariableReturns(
          Vector.tabulate(nbOfMonthsSavings +
          params.nbOfMonthsInRetirement)(i =>
          if (i < nbOfMonthsSavings)
            VariableReturn(i.toString, 0.04 / 12)
          else
            VariableReturn(i.toString, 0.03 /12)))
        val (capitalAtRetirement, capitalAfterDeath) =
          RetCalc.simulatePlan(returns, params, nbOfMonthsSavings)
        capitalAtRetirement should === (541267.1990)
        capitalAfterDeath should === (-57737.7227)
      }
    }
    "Retcalc.nbOfMonthsSaving" should {
      "Tell you the number of months you need to save before you can retire" in {
        val actual = RetCalc.nbOfMonthsSaving(returns=FixedReturns(0.04), params)
        val expected = 23 * 12 + 1
        actual should === (expected)
      }
      "not crash if the resulting nbOfMonths is very high" in {
        val actual = RetCalc.nbOfMonthsSaving(returns = FixedReturns(annualRate = 0.01), params = RetCalcParams(
          nbOfMonthsInRetirement = 40 * 12,
          netIncome = 3000, currentExpenses = 2999,
          initialCapital = 0))
        val expected = 8280
        actual should === (expected)
      }
      "not loop forever if I enter bad parameters" in {
        val actual = RetCalc.nbOfMonthsSaving(returns = FixedReturns(annualRate = 0.04/12), params = RetCalcParams(
         nbOfMonthsInRetirement = 40 * 12,
          netIncome = 1000, currentExpenses = 2000, initialCapital = 10000))
        actual should === (Int.MaxValue)
      }
    }
  }



