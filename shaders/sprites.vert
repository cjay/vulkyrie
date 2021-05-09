#version 450
#extension GL_ARB_separate_shader_objects : enable

layout( push_constant ) uniform Push {
  mat4 transform;
  vec3 pos;
  float turns; // angle in multiples of tau
  vec2 center; // rotation/placement center in sized rectangle
  vec2 size;
  vec2 uvPos;
  vec2 uvSize;
};

// a square with top left at origin and edge length 1
vec2 vertices[6] = vec2[] (
   vec2(0.0, 0.0),
   vec2(1.0, 0.0),
   vec2(1.0, 1.0),
   vec2(1.0, 1.0),
   vec2(0.0, 1.0),
   vec2(0.0, 0.0)
   );

layout(location = 0) out vec2 fragTexCoord;

out gl_PerVertex {
  vec4 gl_Position;
};

float tau = 6.283185307179586;

mat2 rotMat(float ts) {
  float angle = ts * tau;
	float s = sin(angle);
	float c = cos(angle);
	return mat2(
		c, -s,
		s, c
	);
}


void main() {
  gl_Position = transform * vec4(pos.xy + rotMat(turns) * (size * vertices[gl_VertexIndex] - center), pos.z, 1.0);
  fragTexCoord = uvPos + uvSize * vertices[gl_VertexIndex];
}
