#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>

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


float beta = 0.5f;
float alfa = 0.5f;
float rad = 7.0f;
float px, py, pz; 

float cam_z = rad * cos(beta) * cos(alfa);
float cam_x = rad * cos(beta) * sin(alfa);
float cam_y = rad * sin(beta);

void updateCameraPosition() {
	cam_z = rad * cos(beta) * cos(alfa);
	cam_x = rad * cos(beta) * sin(alfa);
	cam_y = rad * sin(beta);
}


void drawCylinder(float radius, float height, int slices) {

// put code to draw cylinder in here

    float angleStep = (360/slices)*M_PI / 180;
	height *= 0.5;

    // Draw the sides of the cylinder
    glBegin(GL_TRIANGLE_STRIP);
	glColor3f(0.0f, 0.0f, 1.0f);
    for (int i = 0; i <= slices; ++i) {
        float angle = i * angleStep;

        px = radius * cos(angle);
		pz = radius * -sin(angle);

        py = height;

        glVertex3f(px, py, pz);       // Bottom circle vertex

		py = -height;

        glVertex3f(px, py, pz);     // Top circle vertex
    }
    glEnd();

    // Draw the bottom circle
    glBegin(GL_TRIANGLE_FAN);
	glColor3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.0f, -height, 0.0f); // Center of the bottom circle
    for (int i = 0; i <= slices; ++i) {
        float angle = i * angleStep;
        px = radius * cos(angle);
        pz = radius * sin(angle);
		
        glVertex3f(px, -height, pz);
    }
    glEnd();

    // Draw the top circle
    glBegin(GL_TRIANGLE_FAN);
	glColor3f(1.0f, 0.0f, 0.0f);
    glVertex3f(0.0f, height, 0); // Center of the top circle
    for (int i = 0; i <= slices; ++i) {
        float angle = i * angleStep;
        px = radius * cos(angle);
        pz = radius * -sin(angle);
        glVertex3f(px, height, pz);
    }
    glEnd();

}

// Or

void drawCylinderWithGLTriangles(float radius, float height, int slices) {
    float angleStep = (360.0f/slices) * M_PI / 180.0f;
    height *= 0.5f;

    // Draw the sides of the cylinder
    glBegin(GL_TRIANGLES);
    for (int i = 0; i < slices; ++i) {
        float angle1 = i * angleStep;
        float angle2 = (i + 1) * angleStep;

        // First vertex coordinates		
        px = radius * cos(angle1);
        pz = radius * -sin(angle1);
        
        // First triangle
        py = height;
        glVertex3f(px, py, pz);    // Top vertex 1
        
        py = -height;
        glVertex3f(px, py, pz);    // Bottom vertex 1
        
        // Second vertex coordinates
        px = radius * cos(angle2);
        pz = radius * -sin(angle2);
        
        py = height;
        glVertex3f(px, py, pz);    // Top vertex 2

        // Second triangle
        glVertex3f(px, py, pz);    // Top vertex 2
        
        px = radius * cos(angle1);
        pz = radius * -sin(angle1);
        py = -height;
        glVertex3f(px, py, pz);    // Bottom vertex 1
        
        px = radius * cos(angle2);
        pz = radius * -sin(angle2);
        glVertex3f(px, py, pz);    // Bottom vertex 2
    

    // Draw the bottom circle
        px = 0.0f;
        py = -height;
        pz = 0.0f;
        glVertex3f(px, py, pz);    // Center

        px = radius * cos(angle1);
        pz = radius * sin(angle1);
        glVertex3f(px, py, pz);    // First point on circumference

        px = radius * cos(angle2);
        pz = radius * sin(angle2);
        glVertex3f(px, py, pz);    // Next point on circumference


    // Draw the top circle
		px = 0.0f;
		py = height;
		pz = 0.0f;
		glVertex3f(px, py, pz);    // Center

		px = radius * cos(angle1);
		pz = radius * -sin(angle1);
		glVertex3f(px, py, pz);    // First point on circumference

		px = radius * cos(angle2);
		pz = radius * -sin(angle2);
		glVertex3f(px, py, pz);    // Next point on circumference
    }
    glEnd();
}


void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(cam_x,cam_y,cam_z, 
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f); 

	glBegin(GL_LINES);
	// X axis in red
	glColor3f(1.0f, 0.0f, 0.0f);
	glVertex3f(-100.0f, 0.0f, 0.0f);
	glVertex3f( 100.0f, 0.0f, 0.0f);
	// Y Axis in Green
	glColor3f(0.0f, 1.0f, 0.0f);
	glVertex3f(0.0f, -100.0f, 0.0f);
	glVertex3f(0.0f, 100.0f, 0.0f);
	// Z Axis in Blue
	glColor3f(0.0f, 0.0f, 1.0f);
	glVertex3f(0.0f, 0.0f, -100.0f);
	glVertex3f(0.0f, 0.0f, 100.0f);
	glEnd();

	//drawCylinder(1, 3, 30);
	drawCylinderWithGLTriangles(1, 3, 30);

	// End of frame
	glutSwapBuffers();
}


void processKeys(unsigned char c, int xx, int yy) {

// put code to process regular keys in here
	switch (c) {
		case 'q':
			exit(0);
			break;

		case '1':
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			break;

		case '2':
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			break;

		case '3':
			glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
			break;

		case '+':
			rad -= 0.1f;
			break;

		case '-':
			rad += 0.1f;
			break;

		default:
			break;
	}

	updateCameraPosition();

	glutPostRedisplay();
}


void processSpecialKeys(int key, int xx, int yy) {

// put code to process special keys in here
	switch (key) {
		case GLUT_KEY_RIGHT:
			alfa += 0.1f;
			break;
		case GLUT_KEY_LEFT:
			alfa -= 0.1f;
			break;
		case GLUT_KEY_UP:
			beta += 0.1f;
			break;
		case GLUT_KEY_DOWN:
			beta -= 0.1f;
			break;
	}
	
	updateCameraPosition();

	glutPostRedisplay();
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
	
// Callback registration for keyboard processing
	glutKeyboardFunc(processKeys);
	glutSpecialFunc(processSpecialKeys);

//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	
// enter GLUT's main cycle
	glutMainLoop();
	
	return 1;
}
