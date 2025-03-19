#include<stdio.h>
#include<stdlib.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <vector>
#include <string>
#include <cmath>


#include <IL/il.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif


float camX = 00, camY = 30, camZ = 40;
int startX, startY, tracking = 0;

float alpha = 0, beta = 45, r = 50;
float a = 0.0f, b = 0.0f;

unsigned int t, tw, th;
unsigned char *imageData;

// buffers is a global variable
// n is the number of buffers - one buffer per array
GLuint vertices, verticeCount;

int timebase;
float frames;

void spherical2Cartesian() {

	camX = r * cos(beta) * sin(alpha);
	camY = r * sin(beta);
	camZ = r * cos(beta) * cos(alpha);
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if(h == 0)
		h = 1;

	// compute window's aspect ratio
	float ratio = w * 1.0 / h;

	// Reset the coordinate system before modifying
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set the correct perspective
	gluPerspective(45,ratio,1,1000);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}


void prepareTerrain() {
    // colocar aqui o código de desnho do terreno usando VBOs com TRIANGLE_STRIPS
	// criar um vector com as coordenadas dos pontos
	std::vector<float> p;

	for (int i = 0; i < tw; i++) {
        for (int j = 0; j < th; j++) {
        	p.push_back(i - tw * 0.5 + 1);
        	p.push_back(imageData[(i + 1) * tw + j]);
        	p.push_back(j - th * 0.5);

        	p.push_back(i - tw * 0.5);
        	p.push_back(imageData[i * tw + j]);
        	p.push_back(j - th * 0.5);

        }
    }

	glGenBuffers(1, &vertices);
	glBindBuffer(GL_ARRAY_BUFFER, vertices);
	
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * p.size(), p.data(), GL_STATIC_DRAW);
	glEnableClientState(GL_VERTEX_ARRAY);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

}


void renderTerrain() {
	glColor3f(0.5f, 0.35f, 0.05f);
	glEnableClientState(GL_VERTEX_ARRAY);

	glBindBuffer(GL_ARRAY_BUFFER, vertices);
	glVertexPointer(3, GL_FLOAT, 0, 0);
	for (int i = 0; i < tw - 1; i++) {
		glDrawArrays(GL_TRIANGLE_STRIP, i * th * 2, th * 2);
	}
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDisableClientState(GL_VERTEX_ARRAY);
}

void createPinkTorus() {
	glPushMatrix();
    glColor3f(1.0f, 0.0f, 1.0f);
    glTranslatef(0.0f, 0.0f, 0.0f);
    glutSolidTorus(0.5, 1.0, 30, 30);
	glPopMatrix();
}


void createCowBoys(float a) {
	glColor3f(0.0f, 0.0f, 1.0f);
    int numBlueTeapots = 8;
    float rc = 15.0f;
    for (int i = 0; i < numBlueTeapots; ++i) {
        float angle = -2.0f * M_PI * i / numBlueTeapots ; // clockwise
        float x = rc * sin(angle + a);
		float z = rc * cos(angle + a);

        glPushMatrix();
        glTranslatef(x, 1.0f, z);
		glRotatef((angle + a) * 180 / M_PI - 90, 0.0f, 1.0f, 0.0f);
        glutSolidTeapot(1.0);
        glPopMatrix();
    }
}



void createIndians(float b) {
	glColor3f(1.0f, 0.0f, 0.0f);
	int numRedTeapots = 16;
    float ri = 35.0f;
    for (int i = 0; i < numRedTeapots; ++i) {
        float angle = 2.0f * M_PI * i / numRedTeapots; // counter-clockwise
        float x = ri * sin(angle + b);
		float z = ri * cos(angle + b);

        glPushMatrix();
        glTranslatef(x, 1.0f, z);
		glRotatef((angle + b) * 180 / M_PI, 0.0f, 1.0f, 0.0f);
        glutSolidTeapot(1.0);
        glPopMatrix();
    }
}

void createTrees() {
	int numTrees = rand() % 250 + 250; // rand - returns a number between 0 and RAND_MAX
	float minR = 50.0f;
    float maxR = 120.0f;
	for (int i = 0; i < numTrees; ++i) {
		float angle = 2.0f * M_PI * rand() / RAND_MAX;
		float distance = minR + (maxR - minR) * rand() / RAND_MAX; 
		float x = distance * sin(angle);
		float z = distance * cos(angle);


		// Tronco
		glPushMatrix();
		glColor3f(0.5f, 0.35f, 0.05f);
		glTranslatef(x, 0.0f, z);
		glRotatef(-90.0f, 1.0f, 0.0f, 0.0f);
		glutSolidCone(1.0, 5.0, 10, 10);
		glPopMatrix();
		
		// Folhas
		glPushMatrix();
    	glColor3f(0.0f, 1.0f, 0.0f);
		glTranslatef(x, 3.0f, z);
		glRotatef(-90.0f, 1.0f, 0.0f, 0.0f);
		glutSolidCone(2.0, 7.0, 20, 20);
		glPopMatrix();
	}

	
}


void renderScene(void) {

	float pos[4] = {-1.0, 1.0, 1.0, 0.0};

	glClearColor(0.0f,0.0f,0.0f,0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();
	gluLookAt(camX, camY, camZ,
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f);



	srand(10); // start the random number sequence

	a -= 0.01f;
	b += 0.01f;
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	createPinkTorus();

	createCowBoys(a);

	createIndians(b);

	createTrees();
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	renderTerrain();

	
	// just so that it renders something before the terrain is built
	// to erase when the terrain is ready


// End of frame
	glutSwapBuffers();

	frames++;
	int time = glutGet(GLUT_ELAPSED_TIME);
	if (time - timebase > 1000) {
		int fps = frames * 1000.0 / (time - timebase);
		timebase = time;
		frames = 0;
		glutSetWindowTitle(std::to_string(fps).c_str());
	}
}


void processKeys(unsigned char key, int xx, int yy) {

// put code to process regular keys in here

	switch (key) {
		case 'q': 
			exit(0); 
			break;

		case '+':
			r -= 1.0f;
			if (r < 1.0f)
				r = 1.0f;
			break;
		
		case '-':
			r += 1.0f;
			break;
		default:
			break;
	}

	spherical2Cartesian();
	glutPostRedisplay();
}



void processMouseButtons(int button, int state, int xx, int yy) {

	if (state == GLUT_DOWN)  {
		startX = xx;
		startY = yy;
		if (button == GLUT_LEFT_BUTTON)
			tracking = 1;
		else if (button == GLUT_RIGHT_BUTTON)
			tracking = 2;
		else
			tracking = 0;
	}
	else if (state == GLUT_UP) {
		if (tracking == 1) {
			alpha += (xx - startX);
			beta += (yy - startY);
		}
		else if (tracking == 2) {

			r -= yy - startY;
			if (r < 3)
				r = 3.0;
		}
		tracking = 0;
	}
}


void processMouseMotion(int xx, int yy) {

	int deltaX, deltaY;
	int alphaAux, betaAux;
	int rAux;

	if (!tracking)
		return;

	deltaX = xx - startX;
	deltaY = yy - startY;

	if (tracking == 1) {


		alphaAux = alpha + deltaX;
		betaAux = beta + deltaY;

		if (betaAux > 85.0)
			betaAux = 85.0;
		else if (betaAux < -85.0)
			betaAux = -85.0;

		rAux = r;
	}
	else if (tracking == 2) {

		alphaAux = alpha;
		betaAux = beta;
		rAux = r - deltaY;
		if (rAux < 3)
			rAux = 3;
	}

	camX = rAux * sin(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
	camZ = rAux * cos(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
	camY = rAux * 							     sin(betaAux * 3.14 / 180.0);

}


void init() {
	ilInit();

	ilGenImages(1,&t);
	ilBindImage(t);
	// terreno.jpg is the image containing our height map
	ilLoadImage((ILstring)"terreno.jpg");
	// convert the image to single channel per pixel
	// with values ranging between 0 and 255
	ilConvertImage(IL_LUMINANCE, IL_UNSIGNED_BYTE);
	// important: check tw and th values
	// both should be equal to 256
	// if not there was an error loading the image
	// most likely the image could not be found
	tw = ilGetInteger(IL_IMAGE_WIDTH);
	th = ilGetInteger(IL_IMAGE_HEIGHT);
	// imageData is a LINEAR array with the pixel values
	imageData = ilGetData();

// 	Build the vertex arrays
	prepareTerrain();


// 	OpenGL settings
	// glEnable(GL_DEPTH_TEST);
	// glEnable(GL_CULL_FACE);
	// glPolygonMode(GL_FRONT, GL_LINE);
}



int main(int argc, char **argv) {

// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(800,800);
	glutCreateWindow("CG@DI-UM");


// Required callback registry
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);

	timebase = glutGet(GLUT_ELAPSED_TIME);

// Callback registration for keyboard processing
	glutKeyboardFunc(processKeys);
	glutMouseFunc(processMouseButtons);
	glutMotionFunc(processMouseMotion);

	glEnable(GL_DEPTH_TEST);
	//glEnable(GL_CULL_FACE);
	spherical2Cartesian();

	glewInit();
	init();


// enter GLUT's main cycle
	glutMainLoop();

	return 0;
}