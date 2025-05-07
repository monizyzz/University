#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include<stdio.h>
#include<cmath>

// distance to target
float dist = 700;
// distance between the eyes
float disp = 30;


float angle=0;
float cBlack[] = {0,0,0,0};
float cWhite[] = {1,1,1,1};
float cRed[] = {0.4,0,0};
float cGreen[] = {0,0.4,0.4};
float cRedish[] = {0.2,0,0};
float cGreenish[] = {0,0.2,0.2};

void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0)
		h = 1;

	float ratio = 1.0* w / h;

	// Reset the coordinate system before modifying
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set the correct perspective.
	gluPerspective(45,ratio,400,10000);
	glMatrixMode(GL_MODELVIEW);
}

void cross(float *a, float *b, float *res) {

	res[0] = a[1]*b[2] - a[2]*b[1];
	res[1] = a[2]*b[0] - a[0]*b[2];
	res[2] = a[0]*b[1] - a[1]*b[0];
}

void normalize(float *a) {

	float l = sqrt(a[0]*a[0] + a[1] * a[1] + a[2] * a[2]);
	a[0] = a[0]/l;
	a[1] = a[1]/l;
	a[2] = a[2]/l;
}

void renderScene(void) {

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();

	float lpos[] = {1,1,0,0};
	float cpos[] = {0,0,dist};

	float up[] = {0,1,0};
	float dir[] = {cpos[0], cpos[1], cpos[2]};
	normalize(dir);
	float right[] = {0,0,0};
	cross(dir, up, right);

	float p[] = {cpos[0] + right[0]*disp, cpos[1] + right[1]*disp, cpos[2] + right[2]*disp};

	// teapot size
	float ts = 150;

	// fill in the code...
	glColorMask(GL_TRUE, GL_FALSE, GL_FALSE, GL_TRUE);
	// red left eye.
	
	glLoadIdentity();
	gluLookAt(cpos[0] + right[0]*disp,
	 		  cpos[1] + right[1]*disp,
	 		  cpos[2] + right[2]*disp,
		      0, 0, 0,
			  0, 1, 0);
	glColorMask(GL_FALSE, GL_TRUE, GL_TRUE, GL_TRUE);

	glutWireTeapot(ts);

	// blue right eye.
	glLoadIdentity();
	gluLookAt(cpos[0] - right[0]*disp,
	 		  cpos[1] - right[1]*disp,
	 		  cpos[2] - right[2]*disp,
		      0, 0, 0,
			  0, 1, 0);


	glutWireTeapot(ts);

	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glutSwapBuffers();

	angle+=0.25;
}




void processNormalKeys(unsigned char key, int x, int y) {

	char s[20];
	if (key == 'q') 
		exit(0);
	switch(key) {
		case 'a' : dist *=.9;break;
		case 's' : dist *= 1.1; break;
	}
	sprintf(s,"%f", dist);
	glutSetWindowTitle(s);

}


int main(int argc, char **argv) {

	// GLUT and window init
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(320,320);
	glutCreateWindow("CG @ DI");

	// GLUT callbacks
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutKeyboardFunc(processNormalKeys);
	
	// OpenGL settings
	glEnable(GL_DEPTH_TEST);
	//glEnable(GL_CULL_FACE);


	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_COLOR_MATERIAL);
	glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);

	glClearColor(0.0f,0.0f,0.0f,0.0f);


	glutMainLoop();

	return 0;
}

