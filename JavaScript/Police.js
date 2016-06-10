let speed = 200;
let ledInterval = 0.3;

moveForward(speed);
while(true) {
  setLed1(0   , 0   , 255   );
  setLed2(0, 0, 0);
  wait(ledInterval);
  setLed1(0, 0, 0);
  setLed2(0, 0, 255);
  wait(ledInterval);
}
