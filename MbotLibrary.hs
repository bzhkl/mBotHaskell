module MbotLibrary(defaultSpeed, getDistance, getLine, leftLed,
      rightLed, forward, backward, turnRight, turnLeft, stopMotor, openMBot, Device, Line(..)) where
  import MBot
  import Prelude hiding (getLine)
  import Data.Bits
  import System.HIDAPI

  defaultSpeed :: Int
  defaultSpeed = 60

  stop :: Int
  stop = 0

  --Get the distance using the ultrasonic sensor
  getDistance :: Device -> IO Float
  getDistance = readUltraSonic

  --Read the line linesensor
  getLine :: Device -> IO Line
  getLine = readLineFollower

  --Set the left led color using rgb notation
  leftLed :: Int -> Int -> Int -> Device -> IO ()
  leftLed = led 1

  --Set the right led color using rgb notation
  rightLed :: Int -> Int -> Int -> Device -> IO ()
  rightLed = led 2

  --Set the led (index) color using rgb notation
  led :: Int -> Int -> Int -> Int -> Device -> IO ()
  led idx r g b d = sendCommand d $ setRGB idx r g b

  --Move the motor forward at the given speed
  forward :: Int -> Device -> IO ()
  forward speed d = do
    sendCommand d $ setMotor rightMotor speed  stop
    sendCommand d $ setMotor leftMotor (complement speed) (complement stop)

  --Move the motor backward at the given speed
  backward :: Int -> Device -> IO ()
  backward speed d = do
    sendCommand d $ setMotor rightMotor (complement speed) (complement stop)
    sendCommand d $ setMotor leftMotor speed  stop

  --Turn right at the given speed
  turnRight :: Int -> Device -> IO ()
  turnRight speed d = do
    sendCommand d $ setMotor rightMotor  stop  stop
    sendCommand d $ setMotor leftMotor  (complement speed)  (complement stop)

  --Turn left at the given speed
  turnLeft :: Int -> Device -> IO ()
  turnLeft speed d = do
    sendCommand d $ setMotor leftMotor  (complement stop) (complement stop)
    sendCommand d $ setMotor rightMotor speed stop

  --Stop the motor
  stopMotor :: Device -> IO ()
  stopMotor d = do
    sendCommand d $ setMotor rightMotor  stop stop
    sendCommand d $ setMotor leftMotor   stop stop
