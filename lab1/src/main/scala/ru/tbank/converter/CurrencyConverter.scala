package ru.tbank.converter

import Errors.{SameCurrencyExchangeException, UnsupportedCurrencyException}

class CurrencyConverter(ratesDictionary: Map[String, Map[String, BigDecimal]]) {
  def exchange(money: Money, toCurrency: String): Money = {
    if (money.currency == toCurrency) {
      throw new SameCurrencyExchangeException()
    }

    ratesDictionary.get(money.currency) match
      case Some(rates)
      => rates.get(toCurrency) match {
        case Some(rates) =>
          var newAmount = money.amount * rates
          Money(newAmount, toCurrency)
        case None => throw new UnsupportedCurrencyException
      }
      case None => throw new UnsupportedCurrencyException
  }
}

object CurrencyConverter {

  import Currencies.SupportedCurrencies

  def apply(ratesDictionary: Map[String, Map[String, BigDecimal]]): CurrencyConverter = {
    val fromCurrencies = ratesDictionary.keys
    val toCurrencies = ratesDictionary.values
    if (
      fromCurrencies.toSet
        .subsetOf(SupportedCurrencies) && toCurrencies.forall(_.keys.toSet.subsetOf(SupportedCurrencies))
    ) new CurrencyConverter(ratesDictionary)
    else throw new UnsupportedCurrencyException
  }
}
