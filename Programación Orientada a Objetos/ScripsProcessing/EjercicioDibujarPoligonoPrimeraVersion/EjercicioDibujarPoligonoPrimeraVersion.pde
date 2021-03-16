void setup(){
  size(1000, 1000);
}

void draw() {
  background(1);
  strokeWeight(5);
  stroke(160, 160, 0);
  circle(500, 500, 8);
  malla(0, 0);
  dibujoPoligonoRegular(500, 500, 15, 80);
  noLoop();
}

void dibujoPoligonoRegular(float x, float y, float l, float ll){
  float angulo = 2*3.14159265/l;
  int i = 1;
  strokeWeight(2);
  while (i <= l){
    strokeWeight(8);
    stroke(0+(i*3), 0+(i*3), 50+(3*i), 255);
    float j = y + sin(angulo*i)*ll;
    float m = x + cos(angulo*i)*ll;
    line(x, y, m, j);
    i += 1;
    println("punto1 = (" + x +"," + y + ")");
    println("punto2 = (" + m +"," + j + ")");
    x = m;
    y = j;
  }
}

void malla(float x, float y) {
  stroke(255, 255, 255, 10);
  float i = 0;
  while (i <= 1000) {
    line(x + i, y, x + i, y + 1000);
    line(x, y + i, x + 1000, y + i   );
    i += 25;
  }
}
