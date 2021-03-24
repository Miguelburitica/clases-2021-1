void setup() {
  size(850, 850);
}

void draw() {
  background(0);
  malla(0, 0);
  stroke(0, 255, 0);
  fill(160,160,50);
  ellipse(500,600,33,33);
  strokeWeight(1);
  dibujarPoligono(mouseX, mouseY, 100, 100);
  fill(160,160,50);
  ellipse(400,700,33,33);
}

void dibujarPoligono(float x, float y, float radio, float lados) {
  pushStyle();
  fill(0,255,255);
  beginShape(POLYGON);
  for (int i = 1; i <= lados; i += 1) {
    float angulo = TWO_PI/lados;
    float x_p = x + cos(angulo*(i+1))*radio;
    float y_p = sin(angulo*(i+1))*radio + y;
    vertex(cos(angulo*i)*radio + x, sin(angulo*i)*radio + y);
    vertex(x_p, y_p);
  }
  endShape(CLOSE);
  
  pushStyle();
  fill(215);
  ellipse(150,450,33,33);
  popStyle();
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
