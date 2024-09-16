package ru.tbank.converter

import ru.tbank.converter.Errors.{CurrencyMismatchException, MoneyAmountShouldBeNonNegativeException, UnsupportedCurrencyException}

import scala.annotation.targetName

case class Money private(amount: BigDecimal, currency: String) {
  def +(other: Money): Money = {
    if (!isSameCurrency(other)) {
      throw new CurrencyMismatchException()
    }
    Money(amount + other.amount, currency)
  }

  def -(other: Money): Money = {
    if (!isSameCurrency(other)) {
      throw new CurrencyMismatchException
    }
    if (amount - other.amount < 0) {
      throw new MoneyAmountShouldBeNonNegativeException
    }
    Money(amount - other.amount, currency)
  }

  def isSameCurrency(other: Money): Boolean = this.currency == other.currency
}

object Money {
  def apply(amount: BigDecimal, currency: String): Money = {
    if (amount < 0) {
      throw new MoneyAmountShouldBeNonNegativeException
    }
    if (!Currencies.SupportedCurrencies.contains(currency)) {
      throw new UnsupportedCurrencyException
    }
    new Money(amount, currency)
  }
}
