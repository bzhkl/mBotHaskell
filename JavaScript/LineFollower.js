let speed = 70;
while(true) {
  let line = MBOT_LINE;
  if((line == "LEFTB")) {
    console.log("Going left");
    moveLeft(speed);
  }
  if((line == "RIGHTB")) {
    console.log("Going right");
    moveRight(speed);
  }
  if((line == "BOTHB")) {
    console.log("Going forward");
    moveForward(speed);
  }
  if((line == "BOTHW")) {
    console.log("Going backward");
    moveBackward(speed);
  }
}
