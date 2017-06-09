#version 130

#include "Uniforms.glsl"
#include "Samplers.glsl"
#include "Transform.glsl"
#include "ScreenPos.glsl"
#include "Lighting.glsl"
#include "Fog.glsl"

#define TILESET_WIDTH 512
#define TILESET_HEIGHT 256
#define TILE_SIZE 64
#define REC_TSX (float(TILE_SIZE)/float(TILESET_WIDTH))
#define REC_TSY (float(TILE_SIZE)/float(TILESET_HEIGHT))
#define REC_TS vec2(REC_TSX, REC_TSY)
#define TILE_OFFSET(x, y) vec2(x * REC_TSX, y * REC_TSY)

varying vec4 vWorldPos;
varying vec3 vNormal;
varying vec2 vTexCoord;

uniform float cTileResolution;
uniform float cChunkSize;

#ifdef PERPIXEL
    #ifdef SHADOW
        #ifndef GL_ES
            varying vec4 vShadowPos[NUMCASCADES];
        #else
            varying highp vec4 vShadowPos[NUMCASCADES];
        #endif
    #endif
    #ifdef SPOTLIGHT
        varying vec4 vSpotPos;
    #endif
    #ifdef POINTLIGHT
        varying vec3 vCubeMaskVec;
    #endif
#else
    varying vec3 vVertexLight;
    varying vec4 vScreenPos;
    #ifdef ENVCUBEMAP
        varying vec3 vReflectionVec;
    #endif
    #if defined(LIGHTMAP) || defined(AO)
        varying vec2 vTexCoord2;
    #endif
#endif

uniform sampler2D sTileMap0;
uniform sampler2DArray sTileSets1;

void VS()
{
    mat4 modelMatrix = iModelMatrix;
    vec3 worldPos = GetWorldPos(modelMatrix);
    gl_Position = GetClipPos(worldPos);
    vNormal = GetWorldNormal(modelMatrix);
    vWorldPos = vec4(worldPos, GetDepth(gl_Position));
    vTexCoord = GetTexCoord(iTexCoord);

    #ifdef PERPIXEL
        // Per-pixel forward lighting
        vec4 projWorldPos = vec4(worldPos, 1.0);

        #ifdef SHADOW
            // Shadow projection: transform from world space to shadow space
            for (int i = 0; i < NUMCASCADES; i++)
                vShadowPos[i] = GetShadowPos(i, vNormal, projWorldPos);
        #endif

        #ifdef SPOTLIGHT
            // Spotlight projection: transform from world space to projector texture coordinates
            vSpotPos = projWorldPos * cLightMatrices[0];
        #endif

        #ifdef POINTLIGHT
            vCubeMaskVec = (worldPos - cLightPos.xyz) * mat3(cLightMatrices[0][0].xyz, cLightMatrices[0][1].xyz, cLightMatrices[0][2].xyz);
        #endif
    #else
        // Ambient & per-vertex lighting
        #if defined(LIGHTMAP) || defined(AO)
            // If using lightmap, disregard zone ambient light
            // If using AO, calculate ambient in the PS
            vVertexLight = vec3(0.0, 0.0, 0.0);
            vTexCoord2 = iTexCoord1;
        #else
            vVertexLight = GetAmbient(GetZonePos(worldPos));
        #endif

        #ifdef NUMVERTEXLIGHTS
            for (int i = 0; i < NUMVERTEXLIGHTS; ++i)
                vVertexLight += GetVertexLight(i, worldPos, vNormal) * cVertexLights[i * 3].rgb;
        #endif

        vScreenPos = GetScreenPos(gl_Position);

        #ifdef ENVCUBEMAP
            vReflectionVec = worldPos - cCameraPos;
        #endif
    #endif
}

// Blend two colors based on alpha
vec4 blend(vec4 c1, vec4 c2) {
  float a = c1.w + c2.w * (1 - c1.w);
  vec3 c = c1.xyz * c1.w + c2.xyz * c2.w * (1 - c1.w);
  return vec4(c/a, a);
  //return vec4(mix(c1.xyz, c2.xyz, c2.w), a);
}

// Blend colors based on sorted order
vec4 orderedBlend(ivec4 sorted, vec4[4] colors) {
  vec4 color = blend(colors[sorted.x], colors[sorted.y]);
  color = blend(color, colors[sorted.z]);
  //color = blend(color, colors[sorted.w]);
  //return color;
  return colors[sorted.w];
  //if (sorted.w == 0) return vec4(0);
  //  else return vec4(1, 0, 0, 1);
}

// Sort 4 values and return then ascending in vector
ivec4 sort4(ivec4 values) {
    int lowest, middle1, middle2, highest, low1, low2, high1, high2;
    int i0 = values.x;
    int i1 = values.y;
    int i2 = values.z;
    int i3 = values.w;

    if (i0 < i1) {
        low1 = i0;
        high1 = i1;
    } else {
        low1 = i1;
        high1 = i0;
    }

    if (i2 < i3) {
        low1 = i2;
        high2 = i3;
    } else {
        low1 = i3;
        high2 = i2;
    }

    if (low1 < low2) {
        lowest = low1;
        middle1 = low2;
    } else {
        lowest = low2;
        middle1 = low1;
    }

    if (high1 > high2) {
        highest = high1;
        middle2 = high2;
    } else {
        highest = high2;
        middle2 = high1;
    }

    if (middle1 < middle2) {
        return ivec4(lowest, middle1, middle2, highest);
    } else {
        return ivec4(lowest, middle2, middle1, highest);
    }
}

// Decide which variant of tiles to use for blending. Type is defined by offset in texture.
vec2[4] chooseTileVariants(ivec4 sorted, ivec4 layers)
{
  int i0 = layers.x;
  int i1 = layers.y;
  int i2 = layers.z;
  int i3 = layers.w;
  vec2 o0, o1, o2, o3;

  // check for fully filled tile
  bool monotile = i0 == i1 && i0 == i2 && i0 == i3;
  // Topleft corner
  if (i0 == sorted.x || monotile) {
    o0 = TILE_OFFSET(0, 0);
  } else if (i0 == i1) {
    if      (i0 == i2) { o0 = TILE_OFFSET(1, 3); }
    else if (i0 == i3) { o0 = TILE_OFFSET(2, 3); }
    else               { o0 = TILE_OFFSET(0, 3); }
  } else if (i0 == i2) {
    if      (i0 == i3) { o0 = TILE_OFFSET(3, 2); }
    else               { o0 = TILE_OFFSET(2, 2); }
  } else if (i0 == i3) {
    o0 = TILE_OFFSET(1, 2);
  } else {
    o0 = TILE_OFFSET(0, 2);
  }
  // Topright corner
  if (i1 == sorted.x || monotile) {
    o1 = TILE_OFFSET(0, 0);
  } else if (i1 == i0) {
    if      (i1 == i2) { o1 = TILE_OFFSET(2, 3); }
    else if (i1 == i3) { o1 = TILE_OFFSET(1, 3); }
    else               { o1 = TILE_OFFSET(0, 3); }
  } else if (i1 == i2) {
    if      (i1 == i3) { o1 = TILE_OFFSET(3, 1); }
    else               { o1 = TILE_OFFSET(2, 1); }
  } else if (i1 == i3) {
    o1 = TILE_OFFSET(1, 1);
  } else {
    o1 = TILE_OFFSET(0, 1);
  }
  // Bottomleft corner
  if (i2 == sorted.x || monotile) {
    o2 = TILE_OFFSET(0, 0);
  } else if (i2 == i0) {
    if      (i2 == i1) { o2 = TILE_OFFSET(2, 3); }
    else if (i2 == i3) { o2 = TILE_OFFSET(3, 2); }
    else               { o2 = TILE_OFFSET(2, 2); }
  } else if (i2 == i1) {
    if      (i2 == i3) { o2 = TILE_OFFSET(3, 1); }
    else               { o2 = TILE_OFFSET(2, 1); }
  } else if (i2 == i3) {
    o2 = TILE_OFFSET(3, 0);
  } else {
    o2 = TILE_OFFSET(2, 0);
  }
  // Bottomright corner
  if (i3 == sorted.x || monotile) {
    o3 = TILE_OFFSET(0, 0);
  } else if (i3 == i0) {
    if      (i3 == i1) { o3 = TILE_OFFSET(1, 3); }
    else if (i3 == i2) { o3 = TILE_OFFSET(3, 2); }
    else               { o3 = TILE_OFFSET(1, 2); }
  } else if (i3 == i1) {
    if      (i3 == i2) { o3 = TILE_OFFSET(3, 1); }
    else               { o3 = TILE_OFFSET(1, 1); }
  } else if (i3 == i2) {
    o3 = TILE_OFFSET(3, 0);
  } else {
    o3 = TILE_OFFSET(1, 0);
  }

  return vec2[4](o0, o1, o2, o3);
}

void PS()
{
    // Find out tile index
    int tix = int(floor(vTexCoord.x))+1;
    int tiy = int(floor(vTexCoord.y))+1;
    // Find position in tile detail map
    float tx = float(tix)/(cChunkSize+1);
    float ty = float(tiy)/(cChunkSize+1);
    // Remove tile offset from UV and transform to match first part of tileset
    vec2 tuv = vTexCoord.xy + 1 - vec2(float(tix), float(tiy));
    tuv.y = 1 - tuv.y;
    tuv = tuv * REC_TS;
    // Find which textures are located at corners of tile (encoded in each rgba channel)
    vec4 tileInfo = texture2D(sTileMap0, vec2(tx, ty));
    ivec4 layers = ivec4(floor(tileInfo*256));

    // Decide which tile variant from tileset to use
    ivec4 sorted = sort4(layers);
    vec2[4] offsets = chooseTileVariants(sorted, layers);
    // Sample tilesets
    vec4 color0 = texture(sTileSets1, vec3(tuv + offsets[0], layers.x));
    vec4 color1 = texture(sTileSets1, vec3(tuv + offsets[1], layers.y));
    vec4 color2 = texture(sTileSets1, vec3(tuv + offsets[2], layers.z));
    vec4 color3 = texture(sTileSets1, vec3(tuv + offsets[3], layers.w));

    // Get material diffuse albedo
    //vec4 blendedColor = orderedBlend(sorted, vec4[4](color0, color1, color2, color3));
    vec4 blendedColor = color3;
    vec4 diffColor = cMatDiffColor * blendedColor;

    // Get material specular albedo
    #ifdef SPECMAP
        vec3 specColor = cMatSpecColor.rgb * texture2D(sSpecMap, vTexCoord.xy).rgb;
    #else
        vec3 specColor = cMatSpecColor.rgb;
    #endif

    // Get normal
    vec3 normal = normalize(vNormal);

    // Get fog factor
    #ifdef HEIGHTFOG
        float fogFactor = GetHeightFogFactor(vWorldPos.w, vWorldPos.y);
    #else
        float fogFactor = GetFogFactor(vWorldPos.w);
    #endif

    #if defined(PERPIXEL)
        // Per-pixel forward lighting
        vec3 lightColor;
        vec3 lightDir;
        vec3 finalColor;

        float diff = GetDiffuse(normal, vWorldPos.xyz, lightDir);

        #ifdef SHADOW
            diff *= GetShadow(vShadowPos, vWorldPos.w);
        #endif

        #if defined(SPOTLIGHT)
            lightColor = vSpotPos.w > 0.0 ? texture2DProj(sLightSpotMap, vSpotPos).rgb * cLightColor.rgb : vec3(0.0, 0.0, 0.0);
        #elif defined(CUBEMASK)
            lightColor = textureCube(sLightCubeMap, vCubeMaskVec).rgb * cLightColor.rgb;
        #else
            lightColor = cLightColor.rgb;
        #endif

        #ifdef SPECULAR
            float spec = GetSpecular(normal, cCameraPosPS - vWorldPos.xyz, lightDir, cMatSpecColor.a);
            finalColor = diff * lightColor * (diffColor.rgb + spec * specColor * cLightColor.a);
        #else
            finalColor = diff * lightColor * diffColor.rgb;
        #endif

        #ifdef AMBIENT
            finalColor += cAmbientColor.rgb * diffColor.rgb;
            finalColor += cMatEmissiveColor;
            gl_FragColor = vec4(GetFog(finalColor, fogFactor), diffColor.a);
        #else
            gl_FragColor = vec4(GetLitFog(finalColor, fogFactor), diffColor.a);
        #endif
    #elif defined(PREPASS)
        // Fill light pre-pass G-Buffer
        float specPower = cMatSpecColor.a / 255.0;

        gl_FragData[0] = vec4(normal * 0.5 + 0.5, specPower);
        gl_FragData[1] = vec4(EncodeDepth(vWorldPos.w), 0.0);
    #elif defined(DEFERRED)
        // Fill deferred G-buffer
        float specIntensity = specColor.g;
        float specPower = cMatSpecColor.a / 255.0;

        gl_FragData[0] = vec4(GetFog(vVertexLight * diffColor.rgb, fogFactor), 1.0);
        gl_FragData[1] = fogFactor * vec4(diffColor.rgb, specIntensity);
        gl_FragData[2] = vec4(normal * 0.5 + 0.5, specPower);
        gl_FragData[3] = vec4(EncodeDepth(vWorldPos.w), 0.0);
    #else
        // Ambient & per-vertex lighting
        vec3 finalColor = vVertexLight * diffColor.rgb;

        #ifdef MATERIAL
            // Add light pre-pass accumulation result
            // Lights are accumulated at half intensity. Bring back to full intensity now
            vec4 lightInput = 2.0 * texture2DProj(sLightBuffer, vScreenPos);
            vec3 lightSpecColor = lightInput.a * lightInput.rgb / max(GetIntensity(lightInput.rgb), 0.001);

            finalColor += lightInput.rgb * diffColor.rgb + lightSpecColor * specColor;
        #endif

        gl_FragColor = vec4(GetFog(finalColor, fogFactor), diffColor.a);
    #endif
}
