#define _USE_MATH_DEFINES
#include <math.h>

struct v3 {
	float x, y, z;

	// Default constructor: initializes the vector to (0, 0, 0)
	v3() : x(0.0f), y(0.0f), z(0.0f) {}

	// Parameterized constructor: initializes the vector to (xx, yy, zz)
	v3(float xx, float yy, float zz) : x(xx), y(yy), z(zz) {}

	// Addition: element-wise addition of two vectors
	v3 operator+(const v3 &other) const {
		return v3(x + other.x, y + other.y, z + other.z);
	}
	
	// Subtraction: element-wise subtraction of two vectors
	v3 operator-(const v3 &other) const {
		return v3(x - other.x, y - other.y, z - other.z);
	}
	
	// Element-wise multiplication of two vectors
	v3 operator*(const v3 &other) const {
		return v3(x * other.x, y * other.y, z * other.z);
	}
	
	// Element-wise division of two vectors.
	// Caution: no check for division by zero is performed.
	v3 operator/(const v3 &other) const {
		return v3(x / other.x, y / other.y, z / other.z);
	}

	// Scalar multiplication: multiply each component by a scalar
	v3 operator*(float scalar) const {
		return v3(x * scalar, y * scalar, z * scalar);
	}
	
	// Scalar division: divide each component by a scalar
	// Caution: ensure scalar is not zero to avoid division by zero.
	v3 operator/(float scalar) const {
		return v3(x / scalar, y / scalar, z / scalar);
	}
};

// Non-member function for scalar multiplication to support commutativity
v3 operator*(float scalar, const v3 &vec) {
	return vec * scalar;
}

v3 cross(const v3 &a, const v3 &b) {
    return v3(
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x
    );
}

// Normalization function: returns the unit vector in the direction of v.
v3 normalize(const v3 &v) {
    float mag = sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
    if (mag == 0.0f)
        return v; // Avoid division by zero
    return v / mag;
}

// ----------------------
// New v2 implementation
// ----------------------
struct v2 {
	float x, y;

	// Default constructor: initializes the vector to (0, 0)
	v2() : x(0.0f), y(0.0f) {}

	// Parameterized constructor: initializes the vector to (xx, yy)
	v2(float xx, float yy) : x(xx), y(yy) {}

	// Addition: element-wise addition of two v2 vectors
	v2 operator+(const v2 &other) const {
		return v2(x + other.x, y + other.y);
	}
	
	// Subtraction: element-wise subtraction of two v2 vectors
	v2 operator-(const v2 &other) const {
		return v2(x - other.x, y - other.y);
	}
	
	// Element-wise multiplication of two v2 vectors
	v2 operator*(const v2 &other) const {
		return v2(x * other.x, y * other.y);
	}
	
	// Element-wise division of two v2 vectors.
	// Caution: no check for division by zero is performed.
	v2 operator/(const v2 &other) const {
		return v2(x / other.x, y / other.y);
	}

	// Scalar multiplication: multiply each component by a scalar
	v2 operator*(float scalar) const {
		return v2(x * scalar, y * scalar);
	}
	
	// Scalar division: divide each component by a scalar
	// Caution: ensure scalar is not zero to avoid division by zero.
	v2 operator/(float scalar) const {
		return v2(x / scalar, y / scalar);
	}
};

// Non-member function for scalar multiplication to support commutativity
v2 operator*(float scalar, const v2 &vec) {
	return vec * scalar;
}

// Normalization function: returns the unit vector in the direction of v.
v2 normalize(const v2 &v) {
	float mag = sqrt(v.x * v.x + v.y * v.y);
	if (mag == 0.0f)
		return v; // Avoid division by zero
	return v / mag;
}