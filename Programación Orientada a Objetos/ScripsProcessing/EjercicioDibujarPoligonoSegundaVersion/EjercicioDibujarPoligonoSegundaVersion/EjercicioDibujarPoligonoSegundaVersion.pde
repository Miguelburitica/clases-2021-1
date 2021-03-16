void setup() { 
  size(1000, 1000);
}

void draw() {
  background(1);
  strokeWeight(5);
  stroke(160, 160, 0);
  circle(500, 500, 8);
  malla(0, 0);
  dibujoPoligonoRegular(500, 500, 13, 80);
  noLoop();
}

void dibujoPoligonoRegular(float x, float y, float lados, float arista) {
  float angulo = TWO_PI/lados;
  int i = 1;
  x += arista/2;
  while (i <= (lados-1)/2) {
    float j = y + sin(angulo*i)*arista;
    i += 1;
    y = j;
  }
  y = 500 - ((y-500)/2);
  i = 1;
  strokeWeight(2);
  while (i <= lados) {
    strokeWeight(8);
    stroke(0+(i*3), 0+(i*3), 50+(3*i), 255);
    float j = y + sin(angulo*i)*arista;
    float m = x + cos(angulo*i)*arista;
    frameRate(1);
    line(x, y, m, j);
    i += 1;
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
