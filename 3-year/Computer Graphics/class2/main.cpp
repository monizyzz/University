#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

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

GLenum polygonMode = GL_LINE; // GL_POINT ou GL_LINE ou GL_FILL

void drawPyramid() { 
    glPolygonMode(GL_FRONT_AND_BACK, polygonMode); 

    glBegin(GL_TRIANGLES);
    
    // Face frontal (vermelho)
    glColor3f(1.0f, 0.0f, 0.0f);
    glVertex3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-0.5f, 0.0f, 0.5f);
    glVertex3f(0.5f, 0.0f, 0.5f);

    // Face direita (verde)
    glColor3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.5f, 0.0f, 0.5f);
    glVertex3f(0.5f, 0.0f, -0.5f);

    // Face traseira (azul)
    glColor3f(0.0f, 0.0f, 1.0f);
    glVertex3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.5f, 0.0f, -0.5f);
    glVertex3f(-0.5f, 0.0f, -0.5f);

    // Face esquerda (amarelo)
    glColor3f(1.0f, 1.0f, 0.0f);
    glVertex3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-0.5f, 0.0f, -0.5f);
    glVertex3f(-0.5f, 0.0f, 0.5f);

    // Base (branco) composta por dois triângulos
	glColor3f(1.0f, 1.0f, 1.0f);

	// Primeiro triângulo
	glVertex3f(-0.5f, 0.0f, 0.5f);
	glVertex3f(0.5f, 0.0f, 0.5f);
	glVertex3f(-0.5f, 0.0f, -0.5f);

	// Segundo triângulo
	glVertex3f(-0.5f, 0.0f, -0.5f);
	glVertex3f(0.5f, 0.0f, 0.5f);
	glVertex3f(0.5f, 0.0f, -0.5f);
	
    glEnd();
}

// Variáveis para transformação da pirâmide
float translateX = 0.0f, translateZ = 0.0f; // Movimento no plano XZ
float rotateY = 0.0f; // Rotação ao redor do eixo Y
float scaleY = 1.0f; // Escala da altura

void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(5.0,5.0,5.0, 
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f);

	// put axis drawing in here
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

	// put the geometric transformations here
    glTranslatef(translateX, 0.0f, translateZ); // Movimento no plano XZ
    glRotatef(rotateY, 0.0f, 1.0f, 0.0f); // Rotação em torno do eixo Y
    glScalef(1.0f, scaleY, 1.0f); // Escalar altura

	// put pyramid drawing instructions here
	drawPyramid();

	// End of frame
	glutSwapBuffers();
}



// write function to process keyboard events
// mover e escalar pirâmide
void keyPressed(unsigned char key, int x, int y) {
	switch (key) {
		case 'q':
			exit(0);
			break;

		case 'w':
			translateZ += 0.1f; // Frente
			break;

		case 's':
			translateZ -= 0.1f; // Trás
			break;

		case 'a':
			translateX -= 0.1f; // Esquerda
			break;

		case 'd':
			translateX += 0.1f; // Direita
			break;

		case '+':
			scaleY += 0.1f; // Aumentar altura
			break;
		
		case '-':
			scaleY -= 0.1f; // Diminuir altura
			break;

		case '1':
			polygonMode = GL_LINE;
			break;

		case '2':
			polygonMode = GL_FILL;
			break;

		case '3':
			polygonMode = GL_POINT;
			break;

		default:
			break;
	}

	glutPostRedisplay();
}

// rotação
void specialKeyPressed(int key_code, int x, int y) {
	switch (key_code) {
		case GLUT_KEY_LEFT:
			rotateY -= 5.0f; // Rotação horária
			break;

		case GLUT_KEY_RIGHT:
			rotateY += 5.0f; // Rotação anti-horária
			break;

		default:
			break;
	}

	glutPostRedisplay();

}

void mouseButton(int button, int state, int x, int y) {
	if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {

		rotateY -= 10.0f;

	} else if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
		
		rotateY += 10.0f;
	}

	glutPostRedisplay();
}

void mousePassive(int x, int y) {
    static int lastX = 0; 

    

    if (x < lastX) {
        // Mouse moved to the left
        rotateY -= 5.0f;
    } else if (x > lastX) {
        // Mouse moved to the right
        rotateY += 5.0f;
    }

    lastX = x; 

    glutPostRedisplay();
}



int main(int argc, char **argv) {

// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("CG@DI-UM");
		
// Required callback registry 
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);

	
// put here the registration of the keyboard callbacks
	glutKeyboardFunc(keyPressed);
	glutSpecialFunc(specialKeyPressed);
	glutMouseFunc(mouseButton);
	glutPassiveMotionFunc(mousePassive);


//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	
// enter GLUT's main cycle
	glutMainLoop();
	
	return 1;
}
