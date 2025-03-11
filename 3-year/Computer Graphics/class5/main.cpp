#include <stdio.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>
#include <stdlib.h>

float alfa = 0.0f, beta = 0.5f, radius = 100.0f;
float camX, camY, camZ;
float a = 0.0f, b = 0.0f;

void spherical2Cartesian() {

	camX = radius * cos(beta) * sin(alfa);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alfa);
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if(h == 0)
		h = 1;

	// compute window's aspect ratio 
	float ratio = w * 1.0 / h;

	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load Identity Matrix
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set perspective
	gluPerspective(45.0f ,ratio, 1.0f ,1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
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
	int numTrees = rand() % 150 + 75; // Número aleatório de árvores entre 75 e 225 | rand - returns a number between 0 and RAND_MAX
	float minR = 50.0f;
    float maxR = 100.0f;
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

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(camX, camY, camZ,
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	glColor3f(0.2f, 0.8f, 0.2f);
	glBegin(GL_TRIANGLES);
		glVertex3f(100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, 100.0f);

		glVertex3f(100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, 100.0f);
		glVertex3f(100.0f, 0, 100.0f);

		glVertex3f(-100.0f, 0, 100.0f);
		glVertex3f(-100.0f, 0, -100.0f);
		glVertex3f(100.0f, 0, -100.0f);

		glVertex3f(100.0f, 0, 100.0f);
		glVertex3f(-100.0f, 0, 100.0f);
		glVertex3f(100.0f, 0, -100.0f);
	glEnd();
	
	// put code to draw scene in here


	// void glRotatef(float ang, float x, float y, float z); // ang in degrees

	srand(100); // start the random number sequence

	a -= 0.01f;
	b += 0.01f;

	createPinkTorus();

	createCowBoys(a);

	createIndians(b);

	createTrees();
	
	glutSwapBuffers();
}


void processKeys(unsigned char c, int xx, int yy) {

// put code to process regular keys in here
	switch (c) {
		case 'q': 
			exit(0); 
			break;

		case '+':
			radius -= 1.0f;
			if (radius < 1.0f)
				radius = 1.0f;
			break;
		
		case '-':
			radius += 1.0f;
			break;
		default:
			break;
	}

	spherical2Cartesian();
	glutPostRedisplay();
}


void processSpecialKeys(int key, int xx, int yy) {

	switch (key) {

	case GLUT_KEY_RIGHT:
		alfa -= 0.1; break;

	case GLUT_KEY_LEFT:
		alfa += 0.1; break;

	case GLUT_KEY_UP:
		beta += 0.1f;
		if (beta > 1.5f)
			beta = 1.5f;
		break;

	case GLUT_KEY_DOWN:
		beta -= 0.1f;
		if (beta < -1.5f)
			beta = -1.5f;
		break;

	case GLUT_KEY_PAGE_DOWN: radius -= 1.0f;
		if (radius < 1.0f)
			radius = 1.0f;
		break;

	case GLUT_KEY_PAGE_UP: radius += 1.0f; break;
	}

	spherical2Cartesian();
	glutPostRedisplay();

}


void printInfo() {

	printf("Vendor: %s\n", glGetString(GL_VENDOR));
	printf("Renderer: %s\n", glGetString(GL_RENDERER));
	printf("Version: %s\n", glGetString(GL_VERSION));

	printf("\nUse Arrows to move the camera up/down and left/right\n");
	printf("Home and End control the distance from the camera to the origin");
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
	glutReshapeFunc(changeSize);
	glutIdleFunc(renderScene);
	
// Callback registration for keyboard processing
	glutKeyboardFunc(processKeys);
	glutSpecialFunc(processSpecialKeys);

//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	spherical2Cartesian();

	printInfo();

// enter GLUT's main cycle
	glutMainLoop();
	
	return 1;
}
