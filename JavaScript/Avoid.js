let speed = 200;
let correctionTime = 0.5;
let direction = true; //true = next time left, false = next time right

while(true) {
  let distance = MBOT_DIST;
  if((distance < 10.0)) {
    console.log("Very close: backward");
    moveBackward(speed);
    wait(correctionTime);
  }
  if((distance < 20.0)) {
    if(direction) {
      console.log("Quite close: left");
      moveLeft(speed);
    }
    else {
      console.log("Quite close: right");
      moveRight(speed);
    }
    wait(correctionTime);
    direction = (!direction);
  }
  else {
    console.log("Safe: forward");
    moveForward(speed);
  }
}
