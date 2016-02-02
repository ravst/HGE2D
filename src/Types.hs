module HGE2D.Types where

import qualified Graphics.Rendering.OpenGL as GL

type Pixel = Double

type Second         = Int
type Millisecond    = Int

type Radian = Double
type Degree = Double


type GlPoint2    = GL.Vertex2 GL.GLfloat
type GlPoint3    = GL.Vector3 GL.GLfloat
type GlVer3      = GL.Vertex3 GL.GLfloat
type GlPosX      = GL.GLfloat
type GlPosY      = GL.GLfloat
type GlPosZ      = GL.GLfloat
type GlWidth     = GL.GLfloat
type GlHeight    = GL.GLfloat
type GlThickness = GL.GLfloat
type GlRadius    = GL.GLfloat
type GlColorRGB  = (GL.GLfloat, GL.GLfloat, GL.GLfloat)
type GlColorRGBA = (GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat)
type GlShape     = [GlPoint2]