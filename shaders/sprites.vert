#version 450
#extension GL_ARB_separate_shader_objects : enable

layout( push_constant ) uniform MVPMatrix {
  mat4 mvpMatrix;
  // vec2 size;
};

vec2 positions[6] = vec2[] (
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

// TODO gl_VertexIndex
void main() {
  gl_Position = mvpMatrix * vec4(positions[gl_VertexIndex], 0.0, 1.0);
  fragTexCoord = positions[gl_VertexIndex];
}
