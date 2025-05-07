cmake_minimum_required(VERSION 2.8)

# Project Name
PROJECT(class13)

if (WIN32)
	set(TOOLKITS_FOLDER "" CACHE PATH "Path to toolkits folder")
	include_directories(${TOOLKITS_FOLDER}/glut)
	link_directories(${TOOLKITS_FOLDER}/glut)
else()		


find_package(GLUT REQUIRED)
include_directories(${GLUT_INCLUDE_DIR})
link_directories(${GLUT_LIBRARY_DIRS})
add_definitions(${GLUT_DEFINITIONS})

find_package(OpenGL REQUIRED)
include_directories(${OpenGL_INCLUDE_DIRS})
link_directories(${OpenGL_LIBRARY_DIRS})
add_definitions(${OpenGL_DEFINITIONS})
endif(WIN32)

add_executable(${PROJECT_NAME} main.cpp)

if (WIN32)
	target_link_libraries(${PROJECT_NAME} ${OPENGL_LIBRARIES} glut32)
	file(COPY ${TOOLKITS_FOLDER}/glut/glut32.dll DESTINATION ${CMAKE_BINARY_DIR})
else(WIN32)
	set( CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} Wno-deprecated")
	target_link_libraries(${PROJECT_NAME} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
endif (WIN32)
