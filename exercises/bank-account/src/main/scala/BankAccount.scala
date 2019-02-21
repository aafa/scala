import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

trait BankAccount {

  def closeAccount(): Unit

  def getBalance: Option[Int]

  def incrementBalance(increment: Int): Option[Int]
}

class CustomerBankAccount extends BankAccount {

  private[this] val money: AtomicInteger = new AtomicInteger(0)
  private[this] val isOpened: AtomicBoolean = new AtomicBoolean(true)

  private[this] def getOnlyIfOpened(value: Int): Option[Int] =
    if (isOpened.get()) {
      Some(value)
    } else {
      None
    }

  override def closeAccount(): Unit = isOpened.set(false)

  override def getBalance: Option[Int] = getOnlyIfOpened(money.intValue())

  override def incrementBalance(increment: Int): Option[Int] =
    getOnlyIfOpened(money.updateAndGet(_ + increment))
}

object Bank {
  def openAccount(): BankAccount = new CustomerBankAccount
}
