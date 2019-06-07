#version 450
#extension GL_ARB_separate_shader_objects : enable

layout( push_constant ) uniform MVPMatrix {
  mat4 mvpMatrix;
};

layout(set = 0, binding = 0) uniform TransformationObject {
  mat4 model;
  mat4 view;
  mat4 proj;
} trans;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inTexCoord;

layout(location = 0) out vec2 fragTexCoord;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    gl_Position = mvpMatrix * vec4(inPosition, 1.0);
    fragTexCoord = inTexCoord;
}
