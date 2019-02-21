object SpaceAge {
  private val secondsOnEarth: Int = 31557600

  private def round2(d: Double): Double = (d * 100.0).round / 100.0

  private def inEarthYears(seconds: Double, ratio: Double): Double = round2(seconds / secondsOnEarth / ratio)

  def onMercury(seconds: Double): Double = inEarthYears(seconds, 0.2408467)

  def onVenus(seconds: Double): Double = inEarthYears(seconds, 0.61519726)

  def onEarth(seconds: Double): Double = inEarthYears(seconds, 1)

  def onMars(seconds: Double): Double = inEarthYears(seconds, 1.8808158)

  def onJupiter(seconds: Double): Double = inEarthYears(seconds, 11.862615)

  def onSaturn(seconds: Double): Double = inEarthYears(seconds, 29.447498)

  def onUranus(seconds: Double): Double = inEarthYears(seconds, 84.016846)

  def onNeptune(seconds: Double): Double = inEarthYears(seconds, 164.79132)

}
