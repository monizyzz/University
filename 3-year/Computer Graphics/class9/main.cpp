#include <stdlib.h>
#include <stdio.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif

#include <math.h>

#define _PI_ 3.14159

#define push_float_vert(v, vertex, x, y, z)\
		v[vertex*3 + 0] = x; \
		v[vertex*3 + 1] = y; \
		v[vertex*3 + 2] = z;

float alfa = 0.0f, beta = 0.0f, radius = 5.0f;
float camX, camY, camZ;

GLuint vertexCount, buffers[2];

int timebase = 0, frame = 0;

void sphericalToCartesian() {

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


void prepareTorus() {
	float *v, *n;
	int stacks = 60;
	int slices = 60;
	float ri = 0.4f;
	float ro = 1.f;
	int vertice_count = slices * stacks * 6;

	v = (float *)malloc(sizeof(float) * vertice_count * 3);
	n = (float *)malloc(sizeof(float) * vertice_count * 3);

	int vertex = 0;

	for (int i = 1; i <= stacks; i++) {
		float last_stack_angle = 2*M_PI / stacks * (i-1);
		float stack_angle = 2*M_PI / stacks * i;

		for (int j = 1; j <= slices; j++) {
			float last_slice_angle = 2*M_PI / slices * (j-1);
			float slice_angle = 2*M_PI / slices * j;

			float rd = (ro - ri) / 2.f;
			float rc = ri + rd;

			float last_ox = rc * cos(last_slice_angle);
			float last_oy = rc * 0.0f;
			float last_oz = rc * -sin(last_slice_angle);

			float ox = rc * cos(slice_angle);
			float oy = rc * 0.0f;
			float oz = rc * -sin(slice_angle);

			float px1 = rd * cos(last_stack_angle) * cos(slice_angle) + ox;
			float py1 = rd * sin(last_stack_angle) + oy;
			float pz1 = rd * cos(last_stack_angle) * -sin(slice_angle) + oz;

			float nx1 = cos(last_stack_angle) * cos(slice_angle);
			float ny1 = sin(last_stack_angle);
			float nz1 = cos(last_stack_angle) * -sin(slice_angle);


			float px2 = rd * cos(stack_angle) * cos(slice_angle) + ox;
			float py2 = rd * sin(stack_angle) + oy;
			float pz2 = rd * cos(stack_angle) * -sin(slice_angle) + oz;

			float nx2 = cos(stack_angle) * cos(slice_angle);
			float ny2 = sin(stack_angle);
			float nz2 = cos(stack_angle) * -sin(slice_angle);


			float px3 = rd * cos(last_stack_angle) * cos(last_slice_angle) + last_ox;
			float py3 = rd * sin(last_stack_angle) + last_oy;
			float pz3 = rd * cos(last_stack_angle) * -sin(last_slice_angle) + last_oz;

			float nx3 = cos(last_stack_angle) * cos(last_slice_angle);
			float ny3 = sin(last_stack_angle);
			float nz3 = cos(last_stack_angle) * -sin(last_slice_angle);


			float px4 = rd * cos(stack_angle) * cos(last_slice_angle) + last_ox;
			float py4 = rd * sin(stack_angle) + last_oy;
			float pz4 = rd * cos(stack_angle) * -sin(last_slice_angle) + last_oz;

			float nx4 = cos(stack_angle) * cos(last_slice_angle);
			float ny4 = sin(stack_angle);
			float nz4 = cos(stack_angle) * -sin(last_slice_angle);

			push_float_vert(v, vertex, px1, py1, pz1);
			push_float_vert(n, vertex, nx1, ny1, nz1);
			vertex++;
			push_float_vert(v, vertex, px2, py2, pz2);
			push_float_vert(n, vertex, nx2, ny2, nz2);
			vertex++;
			push_float_vert(v, vertex, px3, py3, pz3);
			push_float_vert(n, vertex, nx3, ny3, nz3);
			vertex++;

			push_float_vert(v, vertex, px2, py2, pz2);
			push_float_vert(n, vertex, nx2, ny2, nz2);
			vertex++;
			push_float_vert(v, vertex, px4, py4, pz4);
			push_float_vert(n, vertex, nx4, ny4, nz4);
			vertex++;
			push_float_vert(v, vertex, px3, py3, pz3);
			push_float_vert(n, vertex, nx3, ny3, nz3);
			vertex++;
		}
	}

	vertexCount = vertex;

	glGenBuffers(2, buffers);

	glBindBuffer(GL_ARRAY_BUFFER,buffers[0]);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, v,     GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER,buffers[1]);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, n,     GL_STATIC_DRAW);

	free(v);
	free(n);
}


void prepareCilinder(float height, float radius, int sides) {

	float *v, *n;

	v = (float *)malloc(sizeof(float) * 4 * 3 * 3 * sides);
	n = (float *)malloc(sizeof(float) * 4 * 3 * 3 * sides);

	int vertex = 0;
	float delta = 2.0f * _PI_ / sides;

	for (int i = 0; i < sides; ++i) {
		// top
		// central point
		v[vertex*3 + 0] = 0.0f; 
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = 0.0f;

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = 1.0f;
		n[vertex*3 + 2] = 0.0f;


		vertex++;
		v[vertex*3 + 0] = radius * sin( i * delta);
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = radius * cos( i * delta);

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = 1.0f;
		n[vertex*3 + 2] = 0.0f;


		vertex++;
		v[vertex*3 + 0] = radius * sin( (i+1) * delta);
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = radius * cos( (i+1) * delta);

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = 1.0f;
		n[vertex*3 + 2] = 0.0f;

		// body
		// tri�ngulo 1
		vertex++;
		v[vertex*3 + 0] = radius * sin( (i+1) * delta);
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = radius * cos( (i+1) * delta);

		n[vertex*3 + 0] = sin( (i+1) * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( (i+1) * delta);


		vertex++;
		v[vertex*3 + 0] = radius * sin( i * delta);
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = radius * cos( i * delta);

		n[vertex*3 + 0] = sin( i * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( i * delta);


		vertex++;
		v[vertex*3 + 0] = radius * sin( i * delta);
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = radius * cos( i * delta);

		n[vertex*3 + 0] = sin( i * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( i * delta);


		// triangle 2
		vertex++;
		v[vertex*3 + 0] = radius * sin( (i+1) * delta);
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = radius * cos( (i+1) * delta);

		n[vertex*3 + 0] = sin( (i+1) * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( (i+1) * delta);


		vertex++;
		v[vertex*3 + 0] = radius * sin( (i+1) * delta);
		v[vertex*3 + 1] = height /2.0f;
		v[vertex*3 + 2] = radius * cos( (i+1) * delta);

		n[vertex*3 + 0] = sin( (i+1) * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( (i+1) * delta);


		vertex++;
		v[vertex*3 + 0] = radius * sin( i * delta);
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = radius * cos( i * delta);

		n[vertex*3 + 0] = sin( i * delta); 
		n[vertex*3 + 1] = 0.0f;
		n[vertex*3 + 2] = cos( i * delta);

		// base
		// central point
		vertex++;
		v[vertex*3 + 0] = 0.0f; 
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = 0.0f;

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = -1.0f;
		n[vertex*3 + 2] = 0.0f;


		vertex++;
		v[vertex*3 + 0] = radius * sin( (i+1) * delta);
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = radius * cos( (i+1) * delta);

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = -1.0f;
		n[vertex*3 + 2] = 0.0f;


		vertex++;
		v[vertex*3 + 0] = radius * sin( i * delta);
		v[vertex*3 + 1] = -height /2.0f;
		v[vertex*3 + 2] = radius * cos( i * delta);

		n[vertex*3 + 0] = 0.0f; 
		n[vertex*3 + 1] = -1.0f;
		n[vertex*3 + 2] = 0.0f;

		vertex++;
	}

	vertexCount = vertex;

	glGenBuffers(2, buffers);

	glBindBuffer(GL_ARRAY_BUFFER,buffers[0]);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, v,     GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER,buffers[1]);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, n,     GL_STATIC_DRAW);

	free(v);
	free(n);
}


void drawCilinder() {
		
	glBindBuffer(GL_ARRAY_BUFFER,buffers[0]);
	glVertexPointer(3,GL_FLOAT,0,0);

	glBindBuffer(GL_ARRAY_BUFFER,buffers[1]);
	glNormalPointer(GL_FLOAT,0,0);

	glDrawArrays(GL_TRIANGLES, 0, vertexCount);
}


void renderScene(void) {

	float fps;
	int time;
	char s[64];

	glClearColor(0.0f,0.0f,0.0f,0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();

	gluLookAt(camX,camY,camZ, 
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f);

	float pos[4] = {2.0, 2.0, 2.0, 1.0};
	glLightfv(GL_LIGHT0, GL_POSITION, pos);

	float dark[] = { 0.2, 0.2, 0.2, 1.0 };
	float white[] = { 0.8, 0.8, 0.8, 1.0 };
	float red[] = { 0.8, 0.2, 0.2, 1.0 };
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, red);
	glMaterialfv(GL_FRONT, GL_SPECULAR, white);
	glMaterialf(GL_FRONT, GL_SHININESS, 128);

	drawCilinder();

	frame++;
	time=glutGet(GLUT_ELAPSED_TIME); 
	if (time - timebase > 1000) { 
		fps = frame*1000.0/(time-timebase); 
		timebase = time; 
		frame = 0; 
		sprintf(s, "FPS: %f6.2", fps);
		glutSetWindowTitle(s);
	} 

// End of frame
	glutSwapBuffers();
}



void processKeys(int key, int xx, int yy) 
{
	switch(key) {
	
		case GLUT_KEY_RIGHT: 
						alfa -=0.1; break;

		case GLUT_KEY_LEFT: 
						alfa += 0.1; break;

		case GLUT_KEY_UP : 
						beta += 0.1f;
						if (beta > 1.5f)
							beta = 1.5f;
						break;

		case GLUT_KEY_DOWN: 
						beta -= 0.1f; 
						if (beta < -1.5f)
							beta = -1.5f;
						break;

		case GLUT_KEY_PAGE_DOWN : radius -= 0.1f; 
			if (radius < 0.1f)
				radius = 0.1f;
			break;

		case GLUT_KEY_PAGE_UP: radius += 0.1f; break;

	}
	sphericalToCartesian();

}



void initGL() {

// OpenGL settings 
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);

// init
	sphericalToCartesian();
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);

	// prepareCilinder(2,1,24);
	prepareTorus();

	float dark[4] = {0.2, 0.2, 0.2, 1.0};
	float white[4] = {1.0, 1.0, 1.0, 1.0};
	float black[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	// light colors
	glLightfv(GL_LIGHT0, GL_AMBIENT, dark);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, white);
	glLightfv(GL_LIGHT0, GL_SPECULAR, white);
	// controls global ambient light
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, black);
}


int main(int argc, char **argv) {

// init
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(320,320);
	glutCreateWindow("CG@DI-UM");
		
// callback registration
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);

// keyboard callback registration
	glutSpecialFunc(processKeys);

#ifndef __APPLE__	
// init GLEW
	glewInit();
#endif	


	initGL();

	glutMainLoop();
	
	return 1;
}

