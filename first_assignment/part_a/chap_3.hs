---- Questão 3.2 ----

data Temperatura = Celsius | Fahrenheit | Kelvin

converterCelsius :: Double -> Temperatura -> Double
converterCelsius x Celsius = x
converterCelsius x Fahrenheit = (x - 32) * 5/9
converterCelsius x Kelvin = x - 273

converterKelvin :: Double -> Temperatura -> Double
converterKelvin x Celsius = x + 273
converterKelvin x Fahrenheit = converterKelvin (converterCelsius x Fahrenheit) Celsius
converterKelvin x Kelvin = x

converterFahrenheit :: Double -> Temperatura -> Double
converterFahrenheit x Celsius = (x * 9/5) + 32
converterFahrenheit x Fahrenheit = x
converterFahrenheit x Kelvin = converterFahrenheit (converterCelsius x Kelvin) Celsius
