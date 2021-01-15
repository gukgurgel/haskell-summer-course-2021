-- Problem 3.2

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

-- Problem 3.11

data Binario = Zero | Um deriving (Enum, Eq, Show)
data Funcao = Soma2 | Maior | Menor | Mult2
aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Um Um = Zero
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 _ _ = Um

aplicar Mult2 Um Um = Um
aplicar Mult2 _ _ = Zero

aplicar Maior Zero Zero = Zero
aplicar Maior x y = Um

aplicar Menor Um Um = Um
aplicar Menor x y = Zero
