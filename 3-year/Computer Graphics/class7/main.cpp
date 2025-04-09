#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define _USE_MATH_DEFINES
#include <math.h>
#include <stdio.h>
#include <vector>

#include "vector.cpp"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define push_float_vert(verts, x, y, z) {\
							             verts.push_back(x);\
										 verts.push_back(y);\
										 verts.push_back(z);\
								 		}

struct Image {
	int width;
	int height;
	int channels;
	unsigned char* data;
};

// counting frames
int lastTime;
int frame;

// mouse drag
int buttonPressed = false;
v2 initialMousePos;

// Global variables for initial camera angles during a mouse drag.
float initialAlpha;
float initialBeta;

// Global window dimensions
int windowWidth = 800;
int windowHeight = 800;

// Global flag to prevent processing events triggered by glutWarpPointer
bool isWarping = false;

float mouseSensitivity = 0.0002f;

// camera
enum cam_type {
	Fps, Orbital
};

struct cam {
	v3 P = {0, 0, 0}, L = {0, 0, -1}, U = {0, 1, 0};
	float alpha = 0, beta = 0;
	float radius = 100.0f;
};

#define EYE_LEVEL 1
cam camera_orbital;
cam camera_fps;
cam_type currentCameraType = cam_type::Fps;

// trees
#define N_TREES 1000
int treePosX[N_TREES];
int treePosY[N_TREES];
#define R 30

// teapots
#define N_ITEAPOT 10
#define RI 5
#define N_OTEAPOT 20
#define RO 20

float iteapot_angle = 0;
float oteapot_angle = 0;

// terrain
// vbos
GLuint terrainVertices;
Image terrainImage;
int terrainWidth = 200;
int terrainHeight = 200;

Image loadImage(const char* path) {
	Image img;
	img.data = stbi_load(path, &img.width, &img.height, &img.channels, 0);
	
	if (!img.data) {
		printf("Failed to load image: %s\n", path);
	} else {
		printf("Image loaded successfully!\n");
		printf("Width: %d, Height: %d, Channels: %d\n", img.width, img.height, img.channels);
	}
	return img;
}

v2 tex2Coord(int planeWidth, int planeHeight, const Image& img, int x, int y) {
	float sx = -((float)planeWidth / 2);
	float sy = -((float)planeHeight / 2);
	float texToSpaceX = (float)planeWidth / (img.width - 1);
	float texToSpaceY = (float)planeHeight / (img.height - 1);

	float bl_x = sx + x * texToSpaceX;
	float bl_y = sy + (img.height - 1 - y) * texToSpaceY;
	return {bl_x, bl_y};
}

v2 coord2Tex(int planeWidth, int planeHeight, const Image& img, float x, float y) {
	float sx = -((float)planeWidth / 2);
	float sy = -((float)planeHeight / 2);
	float texToSpaceX = (float)planeWidth / (img.width - 1);
	float texToSpaceY = (float)planeHeight / (img.height - 1);

	float tex_x = (x - sx) / texToSpaceX;
	float tex_y = img.height - 1 - ( (y - sy) / texToSpaceY );
	return { tex_x, tex_y };
}

float h(const Image& img, int x, int y) {
	return (float)img.data[(y * img.width + x) * img.channels] * 0.2f;
}

float hf(int planeWidth, int planeHeight, const Image& img, float px, float py) {
	v2 tex = coord2Tex(planeWidth, planeHeight, img, px, py);

	int x1 = floor(tex.x);
	int x2 = x1 + 1;
	int y1 = floor(tex.y);
	int y2 = y1 + 1;

	float fy = tex.y - y1;
	float fx = tex.x - x1;

	float h_x1_y = h(img, x1, y1) * (1-fy) + h(img, x1, y2) * fy;
	float h_x2_y = h(img, x2, y1) * (1-fy) + h(img, x2, y2) * fy;

	float h_xy = h_x1_y * (1-fx) + h_x2_y * fx;

	return h_xy;
}

void vboDrawPlaneTextured(std::vector<float>& verts, int planeWidth, int planeHeight, const Image& img) {
	for (int i = 0; i < img.height - 1; i++) {
		for (int j = 0; j < img.width; j++) {
			v2 bl_xy = tex2Coord(planeWidth, planeHeight, img, j, i+1);
			float bl_z = h(img, j, i+1);

			v2 tl_xy = tex2Coord(planeWidth, planeHeight, img, j, i);
			float tl_z = h(img, j, i);

			push_float_vert(verts, tl_xy.x, tl_xy.y, tl_z);
			push_float_vert(verts, bl_xy.x, bl_xy.y, bl_z);
		}
	}
}

void changeSize(int w, int h)
{
	// Update global window dimensions
    windowWidth = w;
    windowHeight = h;

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if (h == 0)
		h = 1;
	// compute window's aspect ratio
	float ratio = w * 1.0f / h;
	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load the identity matrix
	glLoadIdentity();
	// Set the perspective
	gluPerspective(45.0f, ratio, 1.0f, 1000.0f);
	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);

	// finally set the viewport to be the entire window
	glViewport(0, 0, w, h);
}

void gluLookAtV3(v3 P, v3 L, v3 U) {
	gluLookAt(P.x, P.y, P.z,
		L.x, L.y, L.z,
		U.x, U.y, U.z);
}

void setCameraPos(cam_type type) {
	if (type == cam_type::Orbital) {
		float cx = camera_orbital.radius * cos(camera_orbital.beta) * sin(camera_orbital.alpha);
		float cy = camera_orbital.radius * sin(camera_orbital.beta);
		float cz = camera_orbital.radius * cos(camera_orbital.beta) * cos(camera_orbital.alpha);

		camera_orbital.P.x = cx + camera_orbital.L.x;
		camera_orbital.P.y = cy + camera_orbital.L.y;
		camera_orbital.P.z = cz + camera_orbital.L.z;
	}
	else {
		camera_fps.P.y = EYE_LEVEL + hf(terrainWidth, terrainHeight, terrainImage, camera_fps.P.x, - camera_fps.P.z);

		camera_fps.L.x = camera_fps.P.x + cos(camera_fps.beta) * sin(camera_fps.alpha);
		camera_fps.L.y = camera_fps.P.y - sin(camera_fps.beta);
		camera_fps.L.z = camera_fps.P.z + cos(camera_fps.beta) * cos(camera_fps.alpha);
	}
}

void renderScene(void)
{
	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// set camera using the current camera type
	glLoadIdentity();
	setCameraPos(currentCameraType);
	if (currentCameraType == cam_type::Orbital) {
		gluLookAtV3(camera_orbital.P, camera_orbital.L, camera_orbital.U);
	} else {
		gluLookAtV3(camera_fps.P, camera_fps.L, camera_fps.U);
	}

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	// draw torus
	glColor3f(1, 0.75, 0.8);
	glutSolidTorus(1.0f, 2.0f, 10, 10);

	glPushMatrix();
	glTranslatef(0, 1, 0);

	iteapot_angle -= 2;
	iteapot_angle = fmod(iteapot_angle, 360.0f/N_ITEAPOT);

	glColor3f(0, 0, 1); // Blue color
	// draw inner teapots
	for (int i = 0; i < N_ITEAPOT; i++) {
		glPushMatrix();
		glRotatef(i * 360.0f/N_ITEAPOT + iteapot_angle, 0, 1, 0);
		glTranslatef(RI, 0, 0);
		glutSolidTeapot(1);
		glPopMatrix();
	}

	oteapot_angle += 0.5;
	oteapot_angle = fmod(oteapot_angle, 360.0f/N_OTEAPOT);

	glColor3f(1, 0, 0); // Red color
	// draw outer teapots
	for (int i = 0; i < N_OTEAPOT; i++) {
		glPushMatrix();
		glRotatef(i * 360.0f/N_OTEAPOT + oteapot_angle, 0, 1, 0);
		glTranslatef(RO, 0, 0);
		glRotatef(90, 0, 1, 0);
		glutSolidTeapot(1);
		glPopMatrix();
	}
	glPopMatrix();

	// draw trees
	glPushMatrix();
	glRotatef(-90, 1, 0, 0);
	for (int i = 0; i < N_TREES; i++) {
		// draw tree base
		glPushMatrix();
		float h = hf(terrainWidth, terrainHeight, terrainImage, treePosX[i], treePosY[i]);
		glTranslatef(treePosX[i], treePosY[i], h);

		glColor3f(0.5, 0.25, 0); // Brown color
		glutSolidCone(0.3f, 5.0f, 10, 10);

		// draw tree top
		glPushMatrix();
		glTranslatef(0, 0, 1);

		glColor3f(0.1, 0.5, 0); // Green color
		glutSolidCone(1.0f, 4.0f, 10, 10);

		glPopMatrix();
		glPopMatrix();
	}
	glPopMatrix();

	// draw plane
	// glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glPushMatrix();
	glColor3f(1, 1, 1);
	glRotatef(-90, 1, 0, 0);
	glEnableClientState(GL_VERTEX_ARRAY);
	glBindBuffer(GL_ARRAY_BUFFER, terrainVertices);
	glVertexPointer(3, GL_FLOAT, 0, 0);

	for (int i = 0; i < terrainImage.height - 1 ; i++) {
		glDrawArrays(GL_TRIANGLE_STRIP, (terrainImage.width) * 2 * i, (terrainImage.width) * 2);
	}

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDisableClientState(GL_VERTEX_ARRAY);
	glPopMatrix();

	// End of frame
	glutSwapBuffers();

	// Update timing info (existing code)
	frame++;
	float currentTime = glutGet(GLUT_ELAPSED_TIME);
	float dt = (currentTime - lastTime) / 1000.0f;
	if (currentTime - lastTime > 1000) {
		float fps = frame * 1000.0 / (currentTime - lastTime);
		char title[100];
		sprintf(title, "OpenGL Window - FPS: %.2f - DT: %.3f", fps, dt);
		glutSetWindowTitle(title);
		lastTime = currentTime;
		frame = 0;
	}
	
	lastTime = currentTime;
}

void printInfo() {
	printf("Vendor: %s\n", glGetString(GL_VENDOR));
	printf("Renderer: %s\n", glGetString(GL_RENDERER));
	printf("Version: %s\n", glGetString(GL_VERSION));
}

void keyPress(unsigned char key, int x, int y) {
	if (currentCameraType == cam_type::Fps) {
		v3 D = camera_fps.P - camera_fps.L;
		v3 right = normalize(cross(camera_fps.U, D));
		switch (key) {
			case 's':
				camera_fps.P = camera_fps.P + D * 0.5f;
				break;
			case 'w':
				camera_fps.P = camera_fps.P - D * 0.5f;
				break;
			case 'a':
				camera_fps.P = camera_fps.P - right * 0.5f;
				break;
			case 'd':
				camera_fps.P = camera_fps.P + right * 0.5f;
				break;
		}
	}

	switch (key) {
		case 'c':
			if (currentCameraType == cam_type::Orbital) {
				glutSetCursor(GLUT_CURSOR_NONE);
				glutWarpPointer(windowWidth / 2, windowHeight / 2);
				currentCameraType = cam_type::Fps;
			} else {
				glutSetCursor(GLUT_CURSOR_INHERIT);
				currentCameraType = cam_type::Orbital;
			}
			break;
		case 'q':
			exit(0);
			break;
	}

	glutPostRedisplay();
}

void mouseMove(int x, int y) {
	if (currentCameraType == cam_type::Fps) {
		int centerX = windowWidth / 2;
		int centerY = windowHeight / 2;
		
		// If this event was triggered by our own call to glutWarpPointer, ignore it.
		if (isWarping) {
			isWarping = false;
			return;
		}
		
		// Calculate delta relative to the center of the window.
		int deltaX = x - centerX;
		int deltaY = y - centerY;
		
		// Update camera angles based on the delta.
		camera_fps.alpha -= deltaX * mouseSensitivity;
		camera_fps.beta  += deltaY * mouseSensitivity;
		
		// Clamp beta to avoid flipping.
		if (camera_fps.beta < -M_PI_2 + 0.1f)
			camera_fps.beta = -M_PI_2 + 0.1f;
		else if (camera_fps.beta > M_PI_2 - 0.1f)
			camera_fps.beta = M_PI_2 - 0.1f;
		
		// Set flag to indicate we're warping the pointer.
		isWarping = true;
		glutWarpPointer(centerX, centerY);
	} 
	else { 
		// Orbital mode: use the existing "click-and-drag" behavior.
		if (buttonPressed) {
			v2 currentMousePos = {(float)x, (float)y};
			v2 deltaMousePos = currentMousePos - initialMousePos;
			
			camera_orbital.alpha = initialAlpha - deltaMousePos.x * 0.005f;
			camera_orbital.beta  = initialBeta + deltaMousePos.y * 0.005f;
			
			if (camera_orbital.beta < -M_PI_2 + 0.1f)
				camera_orbital.beta = -M_PI_2 + 0.1f;
			else if (camera_orbital.beta > M_PI_2 - 0.1f)
				camera_orbital.beta = M_PI_2 - 0.1f;
		}
	}
	
	glutPostRedisplay();
}

void mouseClick(int button, int state, int x, int y) {
	cam& camera = (currentCameraType == cam_type::Orbital) ? camera_orbital : camera_fps;
	if (state == GLUT_DOWN && button == GLUT_LEFT_BUTTON) {
		// Store the initial mouse position.
		initialMousePos = {(float)x, (float)y};
		// Store the current camera angles.
		initialAlpha = camera.alpha;
		initialBeta = camera.beta;
		buttonPressed = true;
	}
	else if (state == GLUT_UP && button == GLUT_LEFT_BUTTON) {
		buttonPressed = false;
	}
	else if (button == 3) {
        camera.radius *= 0.99f;
    } else if (button == 4) {
        camera.radius *= 1.01f;
    }
}

int main(int argc, char** argv)
{
	for (int i = 0; i < N_TREES; i++) {
		float randX = 0.0f;
		float randY = 0.0f;
		do {
			randX = (float)rand() / RAND_MAX * terrainWidth - terrainWidth/2;
			randY = (float)rand() / RAND_MAX * terrainHeight - terrainHeight/2;
		} while (randX * randX + randY * randY < R * R);
		treePosX[i] = randX;
		treePosY[i] = randY;
	}

	// put GLUT init here
	glutInit(&argc, argv);
	printInfo();

	// Set display mode (single buffer and RGBA)
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);

	// Set window size and position
	glutInitWindowSize(windowWidth, windowHeight);
	glutInitWindowPosition(100, 100);

	// Create a window with a title
	glutCreateWindow("OpenGL Window");

	// Hide the mouse cursor and center it
	glutSetCursor(GLUT_CURSOR_NONE);
	glutWarpPointer(windowWidth / 2, windowHeight / 2);

	// Register display callback
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene); // to count frames
	glutReshapeFunc(changeSize);
	glutKeyboardFunc(keyPress);
	glutMouseFunc(mouseClick);
	glutMotionFunc(mouseMove);
	glutPassiveMotionFunc(mouseMove);

	// init GLEW
	glewInit();
	glEnableClientState(GL_VERTEX_ARRAY);

	// some OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	
	// enter GLUTs main cycle
	terrainImage = loadImage("../assets/terreno.jpg");
	if (!terrainImage.data) {
		return 0;
	}
	if (terrainImage.width != terrainImage.height) {
		printf("Image must be square for the terrain!\n");
		return 0;
	}

	// Create the vertex buffer for the terrain plane using the image info.
	std::vector<float> vertexB;
	// plane dimensions are separate (here, terrainWidth by terrainHeight units)
	vboDrawPlaneTextured(vertexB, terrainWidth, terrainHeight, terrainImage);

	glGenBuffers(1, &terrainVertices);
	glBindBuffer(GL_ARRAY_BUFFER, terrainVertices);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float)*vertexB.size(), vertexB.data(), GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	// Optionally, free the image data if no longer needed:
	// stbi_image_free(terrainImage.data);

	lastTime = glutGet(GLUT_ELAPSED_TIME);
	glutMainLoop();
	
	return 1;
}
