(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Oct  3 13:30:25 PDT 1995 by najork                   *)
(*       Created on Fri Aug 19 15:27:42 PDT 1994 by najork                   *)

(* Modula-3 version of "gl.h" for Win32. *)

INTERFACE GL;

IMPORT Ctypes;

(*****************************************************************************)
(* Types (complete)                                                          *)
(*****************************************************************************)

TYPE
  GLenum     = Ctypes.unsigned_long;
  GLboolean  = Ctypes.unsigned_char;
  GLbitfield = Ctypes.unsigned_long;
  GLbyte     = Ctypes.signed_char;
  GLshort    = Ctypes.short;
  GLint      = Ctypes.long;
  GLsizei    = Ctypes.long;
  GLubyte    = Ctypes.unsigned_char;
  GLushort   = Ctypes.unsigned_short;
  GLuint     = Ctypes.unsigned_long;
  GLfloat    = Ctypes.float;
  GLclampf   = Ctypes.float;
  GLdouble   = Ctypes.double;
  GLclampd   = Ctypes.double;
(*GLvoid     = Ctypes.void; *)

  GLubyteStar  = UNTRACED REF GLubyte;
  GLdoubleStar = UNTRACED REF GLdouble;
  GLvoidStar   = ADDRESS;

(*****************************************************************************)
(* Constants (complete and identical for POSIX and WIN32)                    *)
(*****************************************************************************)

CONST

  (* AccumOp *)

  GL_ACCUM  = 16_0100;
  GL_LOAD   = 16_0101;
  GL_RETURN = 16_0102;
  GL_MULT   = 16_0103;
  GL_ADD    = 16_0104;

  (* AlphaFunction *)

  GL_NEVER    = 16_0200;
  GL_LESS     = 16_0201;
  GL_EQUAL    = 16_0202;
  GL_LEQUAL   = 16_0203;
  GL_GREATER  = 16_0204;
  GL_NOTEQUAL = 16_0205;
  GL_GEQUAL   = 16_0206;
  GL_ALWAYS   = 16_0207;

  (* AttribMask *)

  GL_CURRENT_BIT         = 16_00000001;
  GL_POINT_BIT           = 16_00000002;
  GL_LINE_BIT            = 16_00000004;
  GL_POLYGON_BIT         = 16_00000008;
  GL_POLYGON_STIPPLE_BIT = 16_00000010;
  GL_PIXEL_MODE_BIT      = 16_00000020;
  GL_LIGHTING_BIT        = 16_00000040;
  GL_FOG_BIT             = 16_00000080;
  GL_DEPTH_BUFFER_BIT    = 16_00000100;
  GL_ACCUM_BUFFER_BIT    = 16_00000200;
  GL_STENCIL_BUFFER_BIT  = 16_00000400;
  GL_VIEWPORT_BIT        = 16_00000800;
  GL_TRANSFORM_BIT       = 16_00001000;
  GL_ENABLE_BIT          = 16_00002000;
  GL_COLOR_BUFFER_BIT    = 16_00004000;
  GL_HINT_BIT            = 16_00008000;
  GL_EVAL_BIT            = 16_00010000;
  GL_LIST_BIT            = 16_00020000;
  GL_TEXTURE_BIT         = 16_00040000;
  GL_SCISSOR_BIT         = 16_00080000;
  GL_ALL_ATTRIB_BITS     = 16_000fffff;

  (* BeginMode *)

  GL_POINTS         = 16_0000;
  GL_LINES          = 16_0001;
  GL_LINE_LOOP      = 16_0002;
  GL_LINE_STRIP     = 16_0003;
  GL_TRIANGLES      = 16_0004;
  GL_TRIANGLE_STRIP = 16_0005;
  GL_TRIANGLE_FAN   = 16_0006;
  GL_QUADS          = 16_0007;
  GL_QUAD_STRIP     = 16_0008;
  GL_POLYGON        = 16_0009;

  (* BlendingFactorDest *)

  GL_ZERO                = 0;
  GL_ONE                 = 1;
  GL_SRC_COLOR           = 16_0300;
  GL_ONE_MINUS_SRC_COLOR = 16_0301;
  GL_SRC_ALPHA           = 16_0302;
  GL_ONE_MINUS_SRC_ALPHA = 16_0303;
  GL_DST_ALPHA           = 16_0304;
  GL_ONE_MINUS_DST_ALPHA = 16_0305;

  (* BlendingFactorSrc *)

  GL_DST_COLOR           = 16_0306;
  GL_ONE_MINUS_DST_COLOR = 16_0307;
  GL_SRC_ALPHA_SATURATE  = 16_0308;

  (* Boolean *)

  GL_TRUE  = 1;
  GL_FALSE = 0;

  (* ClipPlaneName *)

  GL_CLIP_PLANE0 = 16_3000;
  GL_CLIP_PLANE1 = 16_3001;
  GL_CLIP_PLANE2 = 16_3002;
  GL_CLIP_PLANE3 = 16_3003;
  GL_CLIP_PLANE4 = 16_3004;
  GL_CLIP_PLANE5 = 16_3005;

  (* DrawBufferMode *)

  GL_NONE           = 0;
  GL_FRONT_LEFT     = 16_0400;
  GL_FRONT_RIGHT    = 16_0401;
  GL_BACK_LEFT      = 16_0402;
  GL_BACK_RIGHT     = 16_0403;
  GL_FRONT          = 16_0404;
  GL_BACK           = 16_0405;
  GL_LEFT           = 16_0406;
  GL_RIGHT          = 16_0407;
  GL_FRONT_AND_BACK = 16_0408;
  GL_AUX0           = 16_0409;
  GL_AUX1           = 16_040A;
  GL_AUX2           = 16_040B;
  GL_AUX3           = 16_040C;

  (* ErrorCode *)

  GL_NO_ERROR          = 0;
  GL_INVALID_ENUM      = 16_0500;
  GL_INVALID_VALUE     = 16_0501;
  GL_INVALID_OPERATION = 16_0502;
  GL_STACK_OVERFLOW    = 16_0503;
  GL_STACK_UNDERFLOW   = 16_0504;
  GL_OUT_OF_MEMORY     = 16_0505;

  (* FeedBackMode *)

  GL_2D               = 16_0600;
  GL_3D               = 16_0601;
  GL_3D_COLOR         = 16_0602;
  GL_3D_COLOR_TEXTURE = 16_0603;
  GL_4D_COLOR_TEXTURE = 16_0604;

  (* FeedBackToken *)

  GL_PASS_THROUGH_TOKEN = 16_0700;
  GL_POINT_TOKEN        = 16_0701;
  GL_LINE_TOKEN         = 16_0702;
  GL_POLYGON_TOKEN      = 16_0703;
  GL_BITMAP_TOKEN       = 16_0704;
  GL_DRAW_PIXEL_TOKEN   = 16_0705;
  GL_COPY_PIXEL_TOKEN   = 16_0706;
  GL_LINE_RESET_TOKEN   = 16_0707;

  (* FogMode *)

  GL_EXP  = 16_0800;
  GL_EXP2 = 16_0801;

  (* FrontFaceDirection *)

  GL_CW  = 16_0900;
  GL_CCW = 16_0901;

  (* GetMapTarget *)

  GL_COEFF  = 16_0A00;
  GL_ORDER  = 16_0A01;
  GL_DOMAIN = 16_0A02;

  (* GetTarget *)

  GL_CURRENT_COLOR                 = 16_0B00;
  GL_CURRENT_INDEX                 = 16_0B01;
  GL_CURRENT_NORMAL                = 16_0B02;
  GL_CURRENT_TEXTURE_COORDS        = 16_0B03;
  GL_CURRENT_RASTER_COLOR          = 16_0B04;
  GL_CURRENT_RASTER_INDEX          = 16_0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS = 16_0B06;
  GL_CURRENT_RASTER_POSITION       = 16_0B07;
  GL_CURRENT_RASTER_POSITION_VALID = 16_0B08;
  GL_CURRENT_RASTER_DISTANCE       = 16_0B09;
  GL_POINT_SMOOTH                  = 16_0B10;
  GL_POINT_SIZE                    = 16_0B11;
  GL_POINT_SIZE_RANGE              = 16_0B12;
  GL_POINT_SIZE_GRANULARITY        = 16_0B13;
  GL_LINE_SMOOTH                   = 16_0B20;
  GL_LINE_WIDTH                    = 16_0B21;
  GL_LINE_WIDTH_RANGE              = 16_0B22;
  GL_LINE_WIDTH_GRANULARITY        = 16_0B23;
  GL_LINE_STIPPLE                  = 16_0B24;
  GL_LINE_STIPPLE_PATTERN          = 16_0B25;
  GL_LINE_STIPPLE_REPEAT           = 16_0B26;
  GL_LIST_MODE                     = 16_0B30;
  GL_MAX_LIST_NESTING              = 16_0B31;
  GL_LIST_BASE                     = 16_0B32;
  GL_LIST_INDEX                    = 16_0B33;
  GL_POLYGON_MODE                  = 16_0B40;
  GL_POLYGON_SMOOTH                = 16_0B41;
  GL_POLYGON_STIPPLE               = 16_0B42;
  GL_EDGE_FLAG                     = 16_0B43;
  GL_CULL_FACE                     = 16_0B44;
  GL_CULL_FACE_MODE                = 16_0B45;
  GL_FRONT_FACE                    = 16_0B46;
  GL_LIGHTING                      = 16_0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER      = 16_0B51;
  GL_LIGHT_MODEL_TWO_SIDE          = 16_0B52;
  GL_LIGHT_MODEL_AMBIENT           = 16_0B53;
  GL_SHADE_MODEL                   = 16_0B54;
  GL_COLOR_MATERIAL_FACE           = 16_0B55;
  GL_COLOR_MATERIAL_PARAMETER      = 16_0B56;
  GL_COLOR_MATERIAL                = 16_0B57;
  GL_FOG                           = 16_0B60;
  GL_FOG_INDEX                     = 16_0B61;
  GL_FOG_DENSITY                   = 16_0B62;
  GL_FOG_START                     = 16_0B63;
  GL_FOG_END                       = 16_0B64;
  GL_FOG_MODE                      = 16_0B65;
  GL_FOG_COLOR                     = 16_0B66;
  GL_DEPTH_RANGE                   = 16_0B70;
  GL_DEPTH_TEST                    = 16_0B71;
  GL_DEPTH_WRITEMASK               = 16_0B72;
  GL_DEPTH_CLEAR_VALUE             = 16_0B73;
  GL_DEPTH_FUNC                    = 16_0B74;
  GL_ACCUM_CLEAR_VALUE             = 16_0B80;
  GL_STENCIL_TEST                  = 16_0B90;
  GL_STENCIL_CLEAR_VALUE           = 16_0B91;
  GL_STENCIL_FUNC                  = 16_0B92;
  GL_STENCIL_VALUE_MASK            = 16_0B93;
  GL_STENCIL_FAIL                  = 16_0B94;
  GL_STENCIL_PASS_DEPTH_FAIL       = 16_0B95;
  GL_STENCIL_PASS_DEPTH_PASS       = 16_0B96;
  GL_STENCIL_REF                   = 16_0B97;
  GL_STENCIL_WRITEMASK             = 16_0B98;
  GL_MATRIX_MODE                   = 16_0BA0;
  GL_NORMALIZE                     = 16_0BA1;
  GL_VIEWPORT                      = 16_0BA2;
  GL_MODELVIEW_STACK_DEPTH         = 16_0BA3;
  GL_PROJECTION_STACK_DEPTH        = 16_0BA4;
  GL_TEXTURE_STACK_DEPTH           = 16_0BA5;
  GL_MODELVIEW_MATRIX              = 16_0BA6;
  GL_PROJECTION_MATRIX             = 16_0BA7;
  GL_TEXTURE_MATRIX                = 16_0BA8;
  GL_ATTRIB_STACK_DEPTH            = 16_0BB0;
  GL_ALPHA_TEST                    = 16_0BC0;
  GL_ALPHA_TEST_FUNC               = 16_0BC1;
  GL_ALPHA_TEST_REF                = 16_0BC2;
  GL_DITHER                        = 16_0BD0;
  GL_BLEND_DST                     = 16_0BE0;
  GL_BLEND_SRC                     = 16_0BE1;
  GL_BLEND                         = 16_0BE2;
  GL_LOGIC_OP_MODE                 = 16_0BF0;
  GL_LOGIC_OP                      = 16_0BF1;
  GL_AUX_BUFFERS                   = 16_0C00;
  GL_DRAW_BUFFER                   = 16_0C01;
  GL_READ_BUFFER                   = 16_0C02;
  GL_SCISSOR_BOX                   = 16_0C10;
  GL_SCISSOR_TEST                  = 16_0C11;
  GL_INDEX_CLEAR_VALUE             = 16_0C20;
  GL_INDEX_WRITEMASK               = 16_0C21;
  GL_COLOR_CLEAR_VALUE             = 16_0C22;
  GL_COLOR_WRITEMASK               = 16_0C23;
  GL_INDEX_MODE                    = 16_0C30;
  GL_RGBA_MODE                     = 16_0C31;
  GL_DOUBLEBUFFER                  = 16_0C32;
  GL_STEREO                        = 16_0C33;
  GL_RENDER_MODE                   = 16_0C40;
  GL_PERSPECTIVE_CORRECTION_HINT   = 16_0C50;
  GL_POINT_SMOOTH_HINT             = 16_0C51;
  GL_LINE_SMOOTH_HINT              = 16_0C52;
  GL_POLYGON_SMOOTH_HINT           = 16_0C53;
  GL_FOG_HINT                      = 16_0C54;
  GL_TEXTURE_GEN_S                 = 16_0C60;
  GL_TEXTURE_GEN_T                 = 16_0C61;
  GL_TEXTURE_GEN_R                 = 16_0C62;
  GL_TEXTURE_GEN_Q                 = 16_0C63;
  GL_PIXEL_MAP_I_TO_I              = 16_0C70;
  GL_PIXEL_MAP_S_TO_S              = 16_0C71;
  GL_PIXEL_MAP_I_TO_R              = 16_0C72;
  GL_PIXEL_MAP_I_TO_G              = 16_0C73;
  GL_PIXEL_MAP_I_TO_B              = 16_0C74;
  GL_PIXEL_MAP_I_TO_A              = 16_0C75;
  GL_PIXEL_MAP_R_TO_R              = 16_0C76;
  GL_PIXEL_MAP_G_TO_G              = 16_0C77;
  GL_PIXEL_MAP_B_TO_B              = 16_0C78;
  GL_PIXEL_MAP_A_TO_A              = 16_0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE         = 16_0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE         = 16_0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE         = 16_0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE         = 16_0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE         = 16_0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE         = 16_0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE         = 16_0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE         = 16_0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE         = 16_0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE         = 16_0CB9;
  GL_UNPACK_SWAP_BYTES             = 16_0CF0;
  GL_UNPACK_LSB_FIRST              = 16_0CF1;
  GL_UNPACK_ROW_LENGTH             = 16_0CF2;
  GL_UNPACK_SKIP_ROWS              = 16_0CF3;
  GL_UNPACK_SKIP_PIXELS            = 16_0CF4;
  GL_UNPACK_ALIGNMENT              = 16_0CF5;
  GL_PACK_SWAP_BYTES               = 16_0D00;
  GL_PACK_LSB_FIRST                = 16_0D01;
  GL_PACK_ROW_LENGTH               = 16_0D02;
  GL_PACK_SKIP_ROWS                = 16_0D03;
  GL_PACK_SKIP_PIXELS              = 16_0D04;
  GL_PACK_ALIGNMENT                = 16_0D05;
  GL_MAP_COLOR                     = 16_0D10;
  GL_MAP_STENCIL                   = 16_0D11;
  GL_INDEX_SHIFT                   = 16_0D12;
  GL_INDEX_OFFSET                  = 16_0D13;
  GL_RED_SCALE                     = 16_0D14;
  GL_RED_BIAS                      = 16_0D15;
  GL_ZOOM_X                        = 16_0D16;
  GL_ZOOM_Y                        = 16_0D17;
  GL_GREEN_SCALE                   = 16_0D18;
  GL_GREEN_BIAS                    = 16_0D19;
  GL_BLUE_SCALE                    = 16_0D1A;
  GL_BLUE_BIAS                     = 16_0D1B;
  GL_ALPHA_SCALE                   = 16_0D1C;
  GL_ALPHA_BIAS                    = 16_0D1D;
  GL_DEPTH_SCALE                   = 16_0D1E;
  GL_DEPTH_BIAS                    = 16_0D1F;
  GL_MAX_EVAL_ORDER                = 16_0D30;
  GL_MAX_LIGHTS                    = 16_0D31;
  GL_MAX_CLIP_PLANES               = 16_0D32;
  GL_MAX_TEXTURE_SIZE              = 16_0D33;
  GL_MAX_PIXEL_MAP_TABLE           = 16_0D34;
  GL_MAX_ATTRIB_STACK_DEPTH        = 16_0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH     = 16_0D36;
  GL_MAX_NAME_STACK_DEPTH          = 16_0D37;
  GL_MAX_PROJECTION_STACK_DEPTH    = 16_0D38;
  GL_MAX_TEXTURE_STACK_DEPTH       = 16_0D39;
  GL_MAX_VIEWPORT_DIMS             = 16_0D3A;
  GL_SUBPIXEL_BITS                 = 16_0D50;
  GL_INDEX_BITS                    = 16_0D51;
  GL_RED_BITS                      = 16_0D52;
  GL_GREEN_BITS                    = 16_0D53;
  GL_BLUE_BITS                     = 16_0D54;
  GL_ALPHA_BITS                    = 16_0D55;
  GL_DEPTH_BITS                    = 16_0D56;
  GL_STENCIL_BITS                  = 16_0D57;
  GL_ACCUM_RED_BITS                = 16_0D58;
  GL_ACCUM_GREEN_BITS              = 16_0D59;
  GL_ACCUM_BLUE_BITS               = 16_0D5A;
  GL_ACCUM_ALPHA_BITS              = 16_0D5B;
  GL_NAME_STACK_DEPTH              = 16_0D70;
  GL_AUTO_NORMAL                   = 16_0D80;
  GL_MAP1_COLOR_4                  = 16_0D90;
  GL_MAP1_INDEX                    = 16_0D91;
  GL_MAP1_NORMAL                   = 16_0D92;
  GL_MAP1_TEXTURE_COORD_1          = 16_0D93;
  GL_MAP1_TEXTURE_COORD_2          = 16_0D94;
  GL_MAP1_TEXTURE_COORD_3          = 16_0D95;
  GL_MAP1_TEXTURE_COORD_4          = 16_0D96;
  GL_MAP1_VERTEX_3                 = 16_0D97;
  GL_MAP1_VERTEX_4                 = 16_0D98;
  GL_MAP2_COLOR_4                  = 16_0DB0;
  GL_MAP2_INDEX                    = 16_0DB1;
  GL_MAP2_NORMAL                   = 16_0DB2;
  GL_MAP2_TEXTURE_COORD_1          = 16_0DB3;
  GL_MAP2_TEXTURE_COORD_2          = 16_0DB4;
  GL_MAP2_TEXTURE_COORD_3          = 16_0DB5;
  GL_MAP2_TEXTURE_COORD_4          = 16_0DB6;
  GL_MAP2_VERTEX_3                 = 16_0DB7;
  GL_MAP2_VERTEX_4                 = 16_0DB8;
  GL_MAP1_GRID_DOMAIN              = 16_0DD0;
  GL_MAP1_GRID_SEGMENTS            = 16_0DD1;
  GL_MAP2_GRID_DOMAIN              = 16_0DD2;
  GL_MAP2_GRID_SEGMENTS            = 16_0DD3;
  GL_TEXTURE_1D                    = 16_0DE0;
  GL_TEXTURE_2D                    = 16_0DE1;

  (* GetTextureParameter *)

  GL_TEXTURE_WIDTH        = 16_1000;
  GL_TEXTURE_HEIGHT       = 16_1001;
  GL_TEXTURE_COMPONENTS   = 16_1003;
  GL_TEXTURE_BORDER_COLOR = 16_1004;
  GL_TEXTURE_BORDER       = 16_1005;

  (* HintMode *)

  GL_DONT_CARE = 16_1100;
  GL_FASTEST   = 16_1101;
  GL_NICEST    = 16_1102;

  (* LightName *)
    
  GL_LIGHT0 = 16_4000;
  GL_LIGHT1 = 16_4001;
  GL_LIGHT2 = 16_4002;
  GL_LIGHT3 = 16_4003;
  GL_LIGHT4 = 16_4004;
  GL_LIGHT5 = 16_4005;
  GL_LIGHT6 = 16_4006;
  GL_LIGHT7 = 16_4007;

  (* LightParameter *)

  GL_AMBIENT               = 16_1200;
  GL_DIFFUSE               = 16_1201;
  GL_SPECULAR              = 16_1202;
  GL_POSITION              = 16_1203;
  GL_SPOT_DIRECTION        = 16_1204;
  GL_SPOT_EXPONENT         = 16_1205;
  GL_SPOT_CUTOFF           = 16_1206;
  GL_CONSTANT_ATTENUATION  = 16_1207;
  GL_LINEAR_ATTENUATION    = 16_1208;
  GL_QUADRATIC_ATTENUATION = 16_1209;

  (* ListMode *)

  GL_COMPILE             = 16_1300;
  GL_COMPILE_AND_EXECUTE = 16_1301;

  (* ListNameType *)

  GL_BYTE           = 16_1400;
  GL_UNSIGNED_BYTE  = 16_1401;
  GL_SHORT          = 16_1402;
  GL_UNSIGNED_SHORT = 16_1403;
  GL_INT            = 16_1404;
  GL_UNSIGNED_INT   = 16_1405;
  GL_FLOAT          = 16_1406;
  GL_2_BYTES        = 16_1407;
  GL_3_BYTES        = 16_1408;
  GL_4_BYTES        = 16_1409;

  (* LogicOp *)

  GL_CLEAR         = 16_1500;
  GL_AND           = 16_1501;
  GL_AND_REVERSE   = 16_1502;
  GL_COPY          = 16_1503;
  GL_AND_INVERTED  = 16_1504;
  GL_NOOP          = 16_1505;
  GL_XOR           = 16_1506;
  GL_OR            = 16_1507;
  GL_NOR           = 16_1508;
  GL_EQUIV         = 16_1509;
  GL_INVERT        = 16_150A;
  GL_OR_REVERSE    = 16_150B;
  GL_COPY_INVERTED = 16_150C;
  GL_OR_INVERTED   = 16_150D;
  GL_NAND          = 16_150E;
  GL_SET           = 16_150F;

  (* MaterialParameter *)

  GL_EMISSION            = 16_1600;
  GL_SHININESS           = 16_1601;
  GL_AMBIENT_AND_DIFFUSE = 16_1602;
  GL_COLOR_INDEXES       = 16_1603;

  (* MatrixMode *)

  GL_MODELVIEW  = 16_1700;
  GL_PROJECTION = 16_1701;
  GL_TEXTURE    = 16_1702;

  (* PixelCopyType *)

  GL_COLOR   = 16_1800;
  GL_DEPTH   = 16_1801;
  GL_STENCIL = 16_1802;

  (* PixelFormat *)

  GL_COLOR_INDEX     = 16_1900;
  GL_STENCIL_INDEX   = 16_1901;
  GL_DEPTH_COMPONENT = 16_1902;
  GL_RED             = 16_1903;
  GL_GREEN           = 16_1904;
  GL_BLUE            = 16_1905;
  GL_ALPHA           = 16_1906;
  GL_RGB             = 16_1907;
  GL_RGBA            = 16_1908;
  GL_LUMINANCE       = 16_1909;
  GL_LUMINANCE_ALPHA = 16_190A;

  (* PixelType *)

  GL_BITMAP = 16_1A00;

  (* PolygonMode *)

  GL_POINT = 16_1B00;
  GL_LINE  = 16_1B01;
  GL_FILL  = 16_1B02;

  (* RenderingMode *)

  GL_RENDER   = 16_1C00;
  GL_FEEDBACK = 16_1C01;
  GL_SELECT   = 16_1C02;

  (* ShadingModel *)

  GL_FLAT   = 16_1D00;
  GL_SMOOTH = 16_1D01;

  (* StencilOp *)
  GL_KEEP    = 16_1E00;
  GL_REPLACE = 16_1E01;
  GL_INCR    = 16_1E02;
  GL_DECR    = 16_1E03;

  (* StringName *)

  GL_VENDOR     = 16_1F00;
  GL_RENDERER   = 16_1F01;
  GL_VERSION    = 16_1F02;
  GL_EXTENSIONS = 16_1F03;

  (* TextureCoordName *)

  GL_S = 16_2000;
  GL_T = 16_2001;
  GL_R = 16_2002;
  GL_Q = 16_2003;

  (* TextureEnvMode *)

  GL_MODULATE = 16_2100;
  GL_DECAL    = 16_2101;

  (* TextureEnvParameter *)

  GL_TEXTURE_ENV_MODE  = 16_2200;
  GL_TEXTURE_ENV_COLOR = 16_2201;

  (* TextureEnvTarget *)

  GL_TEXTURE_ENV = 16_2300;

  (* TextureGenMode *)

  GL_EYE_LINEAR    = 16_2400;
  GL_OBJECT_LINEAR = 16_2401;
  GL_SPHERE_MAP    = 16_2402;

  (* TextureGenParameter *)

  GL_TEXTURE_GEN_MODE = 16_2500;
  GL_OBJECT_PLANE     = 16_2501;
  GL_EYE_PLANE        = 16_2502;

  (* TextureMagFilter *)

  GL_NEAREST = 16_2600;
  GL_LINEAR  = 16_2601;

  (* TextureMinFilter *)

  GL_NEAREST_MIPMAP_NEAREST = 16_2700;
  GL_LINEAR_MIPMAP_NEAREST  = 16_2701;
  GL_NEAREST_MIPMAP_LINEAR  = 16_2702;
  GL_LINEAR_MIPMAP_LINEAR   = 16_2703;

  (* TextureParameterName *)

  GL_TEXTURE_MAG_FILTER = 16_2800;
  GL_TEXTURE_MIN_FILTER = 16_2801;
  GL_TEXTURE_WRAP_S     = 16_2802;
  GL_TEXTURE_WRAP_T     = 16_2803;

  (* TextureWrapMode *)
  
  GL_CLAMP  = 16_2900;
  GL_REPEAT = 16_2901;


(*****************************************************************************)
(* Procedures                                                                *)
(*****************************************************************************)

<*EXTERNAL glAccum:WINAPI*> 
PROCEDURE glAccum (op: GLenum; value: GLfloat);

<*EXTERNAL glAlphaFunc:WINAPI*> 
PROCEDURE glAlphaFunc (func: GLenum; ref: GLclampf);

<*EXTERNAL glBegin:WINAPI*> 
PROCEDURE glBegin (mode: GLenum);

<*EXTERNAL glBitmap:WINAPI*> 
PROCEDURE glBitmap (width : GLsizei; 
                    height: GLsizei; 
                    xorig : GLfloat; 
                    yorig : GLfloat; 
                    xmove : GLfloat; 
                    ymove : GLfloat; 
                    bitmap: UNTRACED REF ARRAY OF GLubyte);

<*EXTERNAL glBlendFunc:WINAPI*> 
PROCEDURE glBlendFunc (sfactor, dfactor: GLenum);

<*EXTERNAL glCallList:WINAPI*> 
PROCEDURE glCallList (list: GLuint);

<*EXTERNAL glCallLists:WINAPI*> 
PROCEDURE glCallLists (n: GLsizei; type: GLenum; lists: ADDRESS);

<*EXTERNAL glClear:WINAPI*> 
PROCEDURE glClear (mask: GLbitfield);

<*EXTERNAL glClearAccum:WINAPI*> 
PROCEDURE glClearAccum (red, green, blue, alpha: GLfloat);

<*EXTERNAL glClearColor:WINAPI*> 
PROCEDURE glClearColor (red, green, blue, alpha: GLclampf);

<*EXTERNAL glClearDepth:WINAPI*> 
PROCEDURE glClearDepth (depth: GLclampd);

<*EXTERNAL glClearIndex:WINAPI*> 
PROCEDURE glClearIndex (c: GLfloat);

<*EXTERNAL glClearStencil:WINAPI*> 
PROCEDURE glClearStencil (s: GLint);

<*EXTERNAL glClipPlane:WINAPI*> 
PROCEDURE glClipPlane (plane   : GLenum; 
                       equation: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glColor3b:WINAPI*> 
PROCEDURE glColor3b (red, green, blue: GLbyte);

<*EXTERNAL glColor3bv:WINAPI*> 
PROCEDURE glColor3bv (v: UNTRACED REF ARRAY OF GLbyte);

<*EXTERNAL glColor3d:WINAPI*> 
PROCEDURE glColor3d (red, green, blue: GLdouble);

<*EXTERNAL glColor3dv:WINAPI*> 
PROCEDURE glColor3dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glColor3f:WINAPI*> 
PROCEDURE glColor3f (red, green, blue : GLfloat);

<*EXTERNAL glColor3fv:WINAPI*> 
PROCEDURE glColor3fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glColor3i:WINAPI*> 
PROCEDURE glColor3i (red, green, blue: GLint);

<*EXTERNAL glColor3iv:WINAPI*> 
PROCEDURE glColor3iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glColor3s:WINAPI*> 
PROCEDURE glColor3s (red, green, blue: GLshort);

<*EXTERNAL glColor3sv:WINAPI*> 
PROCEDURE glColor3sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glColor3ub:WINAPI*> 
PROCEDURE glColor3ub (red, green, blue: GLubyte);

<*EXTERNAL glColor3ubv:WINAPI*> 
PROCEDURE glColor3ubv (v: UNTRACED REF ARRAY OF GLubyte);

<*EXTERNAL glColor3ui:WINAPI*> 
PROCEDURE glColor3ui (red, green, blue: GLuint);

<*EXTERNAL glColor3uiv:WINAPI*> 
PROCEDURE glColor3uiv (v: UNTRACED REF ARRAY OF GLuint);

<*EXTERNAL glColor3us:WINAPI*> 
PROCEDURE glColor3us (red, green, blue: GLushort);

<*EXTERNAL glColor3usv:WINAPI*> 
PROCEDURE glColor3usv (v: UNTRACED REF ARRAY OF GLushort);

<*EXTERNAL glColor4b:WINAPI*> 
PROCEDURE glColor4b (red, green, blue, alpha: GLbyte);

<*EXTERNAL glColor4bv:WINAPI*> 
PROCEDURE glColor4bv (v: UNTRACED REF ARRAY OF GLbyte);

<*EXTERNAL glColor4d:WINAPI*> 
PROCEDURE glColor4d (red, green, blue, alpha: GLdouble);

<*EXTERNAL glColor4dv:WINAPI*> 
PROCEDURE glColor4dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glColor4f:WINAPI*> 
PROCEDURE glColor4f (red, green, blue, alpha: GLfloat);

<*EXTERNAL glColor4fv:WINAPI*> 
PROCEDURE glColor4fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glColor4i:WINAPI*> 
PROCEDURE glColor4i (red, green, blue, alpha: GLint);

<*EXTERNAL glColor4iv:WINAPI*> 
PROCEDURE glColor4iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glColor4s:WINAPI*> 
PROCEDURE glColor4s (red, green, blue, alpha: GLshort);

<*EXTERNAL glColor4sv:WINAPI*> 
PROCEDURE glColor4sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glColor4ub:WINAPI*> 
PROCEDURE glColor4ub (red, green, blue, alpha: GLubyte);

<*EXTERNAL glColor4ubv:WINAPI*> 
PROCEDURE glColor4ubv (v: UNTRACED REF ARRAY OF GLubyte);

<*EXTERNAL glColor4ui:WINAPI*> 
PROCEDURE glColor4ui (red, green, blue, alpha: GLuint);

<*EXTERNAL glColor4uiv:WINAPI*> 
PROCEDURE glColor4uiv (v: UNTRACED REF ARRAY OF GLuint);

<*EXTERNAL glColor4us:WINAPI*> 
PROCEDURE glColor4us (red, green, blue, alpha: GLushort);

<*EXTERNAL glColor4usv:WINAPI*> 
PROCEDURE glColor4usv (v: UNTRACED REF ARRAY OF GLushort);

<*EXTERNAL glColorMask:WINAPI*> 
PROCEDURE glColorMask (red, green, blue, alpha: GLboolean);

<*EXTERNAL glColorMaterial:WINAPI*> 
PROCEDURE glColorMaterial (face, mode: GLenum);

<*EXTERNAL glCopyPixels:WINAPI*> 
PROCEDURE glCopyPixels (x, y         : GLint; 
                        width, height: GLsizei; 
                        type         : GLenum);

<*EXTERNAL glCullFace:WINAPI*> 
PROCEDURE glCullFace (mode: GLenum);

<*EXTERNAL glDeleteLists:WINAPI*> 
PROCEDURE glDeleteLists (list: GLuint; range: GLsizei);

<*EXTERNAL glDepthFunc:WINAPI*> 
PROCEDURE glDepthFunc (func: GLenum);

<*EXTERNAL glDepthMask:WINAPI*> 
PROCEDURE glDepthMask (flag: GLboolean);

<*EXTERNAL glDepthRange:WINAPI*> 
PROCEDURE glDepthRange (near, far: GLclampd);

<*EXTERNAL glDisable:WINAPI*> 
PROCEDURE glDisable (cap : GLenum);

<*EXTERNAL glDrawBuffer:WINAPI*> 
PROCEDURE glDrawBuffer (mode : GLenum);

<*EXTERNAL glDrawPixels:WINAPI*> 
PROCEDURE glDrawPixels (width, height: GLsizei; 
                        format, type : GLenum;
                        pixels       : ADDRESS);

<*EXTERNAL glEdgeFlag:WINAPI*> 
PROCEDURE glEdgeFlag (flag: GLboolean);

<*EXTERNAL glEdgeFlagv:WINAPI*> 
PROCEDURE glEdgeFlagv (flag: UNTRACED REF ARRAY OF GLboolean);

<*EXTERNAL glEnable:WINAPI*> 
PROCEDURE glEnable (cap : GLenum);

<*EXTERNAL glEnd:WINAPI*> 
PROCEDURE glEnd ();

<*EXTERNAL glEndList:WINAPI*> 
PROCEDURE glEndList ();

<*EXTERNAL glEvalCoord1d:WINAPI*> 
PROCEDURE glEvalCoord1d (u: GLdouble);

<*EXTERNAL glEvalCoord1dv:WINAPI*> 
PROCEDURE glEvalCoord1dv (u: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glEvalCoord1f:WINAPI*> 
PROCEDURE glEvalCoord1f (u: GLfloat);

<*EXTERNAL glEvalCoord1fv:WINAPI*> 
PROCEDURE glEvalCoord1fv (u: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glEvalCoord2d:WINAPI*> 
PROCEDURE glEvalCoord2d (u, v: GLdouble);

<*EXTERNAL glEvalCoord2dv:WINAPI*> 
PROCEDURE glEvalCoord2dv (u: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glEvalCoord2f:WINAPI*> 
PROCEDURE glEvalCoord2f (u, v: GLfloat);

<*EXTERNAL glEvalCoord2fv:WINAPI*> 
PROCEDURE glEvalCoord2fv (u: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glEvalMesh1:WINAPI*> 
PROCEDURE glEvalMesh1 (mode: GLenum; i1, i2: GLint);

<*EXTERNAL glEvalMesh2:WINAPI*> 
PROCEDURE glEvalMesh2 (mode : GLenum; i1, i2, j1, j2 : GLint);

<*EXTERNAL glEvalPoint1:WINAPI*> 
PROCEDURE glEvalPoint1 (i: GLint);

<*EXTERNAL glEvalPoint2:WINAPI*> 
PROCEDURE glEvalPoint2 (i, j: GLint);

<*EXTERNAL glFeedbackBuffer:WINAPI*> 
PROCEDURE glFeedbackBuffer (size  : GLsizei; 
                            type  : GLenum; 
                            buffer: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glFinish:WINAPI*> 
PROCEDURE glFinish ();

<*EXTERNAL glFlush:WINAPI*> 
PROCEDURE glFlush ();

<*EXTERNAL glFogf:WINAPI*> 
PROCEDURE glFogf (pname: GLenum; param: GLfloat);

<*EXTERNAL glFogfv:WINAPI*> 
PROCEDURE glFogfv (pname: GLenum; params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glFogi:WINAPI*> 
PROCEDURE glFogi (pname: GLenum; param: GLint);

<*EXTERNAL glFogiv:WINAPI*> 
PROCEDURE glFogiv (pname: GLenum; params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glFrontFace:WINAPI*> 
PROCEDURE glFrontFace (mode: GLenum);

<*EXTERNAL glFrustum:WINAPI*> 
PROCEDURE glFrustum (left, right, bottom, top, near, far: GLdouble);

<*EXTERNAL glGenLists:WINAPI*> 
PROCEDURE glGenLists (range: GLsizei): GLuint;

<*EXTERNAL glGetBooleanv:WINAPI*> 
PROCEDURE glGetBooleanv (pname: GLenum; 
                         params: UNTRACED REF ARRAY OF GLboolean);

<*EXTERNAL glGetClipPlane:WINAPI*> 
PROCEDURE glGetClipPlane (pname   : GLenum; 
                          equation: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glGetDoublev:WINAPI*> 
PROCEDURE glGetDoublev (pname : GLenum; 
                        params: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glGetError:WINAPI*> 
PROCEDURE glGetError (): GLenum;

<*EXTERNAL glGetFloatv:WINAPI*> 
PROCEDURE glGetFloatv (pname : GLenum; 
                       params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetIntegerv:WINAPI*> 
PROCEDURE glGetIntegerv (pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetLightfv:WINAPI*>
PROCEDURE glGetLightfv (light : GLenum; 
                        pname : GLenum; 
                        params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetLightiv:WINAPI*>
PROCEDURE glGetLightiv (light : GLenum; 
                        pname : GLenum; 
                        params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetMapdv:WINAPI*>
PROCEDURE glGetMapdv (target: GLenum; 
                      query : GLenum; 
                      v     : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glGetMapfv:WINAPI*>
PROCEDURE glGetMapfv (target: GLenum; 
                      query : GLenum; 
                      v     : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetMapiv:WINAPI*>
PROCEDURE glGetMapiv (target: GLenum; 
                      query : GLenum; 
                      v     : UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetMaterialfv:WINAPI*> 
PROCEDURE glGetMaterialfv (face  : GLenum; 
                           pname : GLenum; 
                           params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetMaterialiv:WINAPI*> 
PROCEDURE glGetMaterialiv (face  : GLenum; 
                           pname : GLenum; 
                           params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetPixelMapfv:WINAPI*> 
PROCEDURE glGetPixelMapfv (map   : GLenum; 
                           values: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetPixelMapuiv:WINAPI*> 
PROCEDURE glGetPixelMapuiv (map   : GLenum; 
                            values: UNTRACED REF ARRAY OF GLuint);

<*EXTERNAL glGetPixelMapusv:WINAPI*> 
PROCEDURE glGetPixelMapusv (map   : GLenum;
                            values: UNTRACED REF ARRAY OF GLushort);

<*EXTERNAL glGetPolygonStipple:WINAPI*> 
PROCEDURE glGetPolygonStipple (mask: UNTRACED REF ARRAY OF GLubyte);

<*EXTERNAL glGetString:WINAPI*> 
PROCEDURE glGetString (name: GLenum): UNTRACED REF ARRAY OF GLubyte;

<*EXTERNAL glGetTexEnvfv:WINAPI*> 
PROCEDURE glGetTexEnvfv (target: GLenum; 
                         pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetTexEnviv:WINAPI*> 
PROCEDURE glGetTexEnviv (target: GLenum; 
                         pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetTexGendv:WINAPI*> 
PROCEDURE glGetTexGendv (coord : GLenum; 
                         pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glGetTexGenfv:WINAPI*> 
PROCEDURE glGetTexGenfv (coord : GLenum; 
                         pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetTexGeniv:WINAPI*> 
PROCEDURE glGetTexGeniv (coord : GLenum; 
                         pname : GLenum; 
                         params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetTexImage:WINAPI*> 
PROCEDURE glGetTexImage (target: GLenum;
                         level : GLint;
                         format: GLenum; 
                         type  : GLenum; 
                         pixels: ADDRESS);

<*EXTERNAL glGetTexLevelParameterfv:WINAPI*> 
PROCEDURE glGetTexLevelParameterfv (target: GLenum; 
                                    level : GLint;
                                    pname : GLenum;
                                    params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetTexLevelParameteriv:WINAPI*> 
PROCEDURE glGetTexLevelParameteriv (target: GLenum; 
                                    level : GLint; 
                                    pname : GLenum; 
                                    params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glGetTexParameterfv:WINAPI*> 
PROCEDURE glGetTexParameterfv (target: GLenum; 
                               pname : GLenum; 
                               params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glGetTexParameteriv:WINAPI*> 
PROCEDURE glGetTexParameteriv (target: GLenum; 
                               pname : GLenum; 
                               params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glHint:WINAPI*> 
PROCEDURE glHint (target: GLenum; mode: GLenum);

<*EXTERNAL glIndexMask:WINAPI*> 
PROCEDURE glIndexMask (mask: GLuint);

<*EXTERNAL glIndexd:WINAPI*> 
PROCEDURE glIndexd (c: GLdouble);

<*EXTERNAL glIndexdv:WINAPI*> 
PROCEDURE glIndexdv (c: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glIndexf:WINAPI*> 
PROCEDURE glIndexf (c: GLfloat);

<*EXTERNAL glIndexfv:WINAPI*> 
PROCEDURE glIndexfv (c: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glIndexi:WINAPI*> 
PROCEDURE glIndexi (c: GLint);

<*EXTERNAL glIndexiv:WINAPI*> 
PROCEDURE glIndexiv (c: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glIndexs:WINAPI*> 
PROCEDURE glIndexs (c: GLshort);

<*EXTERNAL glIndexsv:WINAPI*> 
PROCEDURE glIndexsv (c: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glInitNames:WINAPI*> 
PROCEDURE glInitNames ();

<*EXTERNAL glIsEnabled:WINAPI*> 
PROCEDURE glIsEnabled (cap: GLenum): GLboolean;

<*EXTERNAL glIsList:WINAPI*> 
PROCEDURE glIsList (list: GLuint): GLboolean;

<*EXTERNAL glLightModelf:WINAPI*> 
PROCEDURE glLightModelf (pname: GLenum; param: GLfloat);

<*EXTERNAL glLightModelfv:WINAPI*> 
PROCEDURE glLightModelfv (pname : GLenum; 
                          params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glLightModeli:WINAPI*> 
PROCEDURE glLightModeli (pname: GLenum; param: GLint);

<*EXTERNAL glLightModeliv:WINAPI*> 
PROCEDURE glLightModeliv (pname: GLenum; params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glLightf:WINAPI*> 
PROCEDURE glLightf (light: GLenum; pname: GLenum; param: GLfloat);

<*EXTERNAL glLightfv:WINAPI*> 
PROCEDURE glLightfv (light : GLenum; 
                     pname : GLenum; 
                     params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glLighti:WINAPI*> 
PROCEDURE glLighti (light: GLenum; pname: GLenum; param: GLint);

<*EXTERNAL glLightiv:WINAPI*> 
PROCEDURE glLightiv (light : GLenum; 
                     pname : GLenum; 
                     params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glLineStipple:WINAPI*> 
PROCEDURE glLineStipple (factor: GLint; pattern: GLushort);

<*EXTERNAL glLineWidth:WINAPI*> 
PROCEDURE glLineWidth (width: GLfloat);

<*EXTERNAL glListBase:WINAPI*> 
PROCEDURE glListBase (base: GLuint);

<*EXTERNAL glLoadIdentity:WINAPI*> 
PROCEDURE glLoadIdentity ();

<*EXTERNAL glLoadMatrixd:WINAPI*> 
PROCEDURE glLoadMatrixd (m: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glLoadMatrixf:WINAPI*> 
PROCEDURE glLoadMatrixf (m: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glLoadName:WINAPI*> 
PROCEDURE glLoadName (name: GLuint);

<*EXTERNAL glLogicOp:WINAPI*> 
PROCEDURE glLogicOp (opcode: GLenum);

<*EXTERNAL glMap1d:WINAPI*> 
PROCEDURE glMap1d (target       : GLenum; 
                   u1, u2       : GLdouble;
                   stride, order: GLint;
                   points       : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glMap1f:WINAPI*> 
PROCEDURE glMap1f (target       : GLenum; 
                   u1, u2       : GLfloat;
                   stride, order: GLint;
                   points       : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glMap2d:WINAPI*> 
PROCEDURE glMap2d (target         : GLenum; 
                   u1, u2         : GLdouble; 
                   ustride, uorder: GLint;
                   v1, v2         : GLdouble; 
                   vstride, vorder: GLint;
                   points         : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glMap2f:WINAPI*> 
PROCEDURE glMap2f (target         : GLenum; 
                   u1, u2         : GLfloat;
                   ustride, uorder: GLint;
                   v1, v2         : GLfloat; 
                   vstride, vorder: GLint;
                   points         : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glMapGrid1d:WINAPI*> 
PROCEDURE glMapGrid1d (un: GLint; u1, u2: GLdouble);

<*EXTERNAL glMapGrid1f:WINAPI*> 
PROCEDURE glMapGrid1f (un: GLint; u1, u2: GLfloat);

<*EXTERNAL glMapGrid2d:WINAPI*> 
PROCEDURE glMapGrid2d (un: GLint; u1, u2: GLdouble; 
                       vn: GLint; v1, v2: GLdouble);

<*EXTERNAL glMapGrid2f:WINAPI*> 
PROCEDURE glMapGrid2f (un: GLint; u1, u2 : GLfloat; 
                                    vn: GLint; v1, v2 : GLfloat);

<*EXTERNAL glMaterialf:WINAPI*> 
PROCEDURE glMaterialf (face, pname: GLenum; param: GLfloat);

<*EXTERNAL glMaterialfv:WINAPI*> 
PROCEDURE glMaterialfv (face, pname: GLenum; 
                        params     : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glMateriali:WINAPI*> 
PROCEDURE glMateriali (face, pname: GLenum; param: GLint);

<*EXTERNAL glMaterialiv:WINAPI*> 
PROCEDURE glMaterialiv (face, pname: GLenum; 
                        params     : UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glMatrixMode:WINAPI*> 
PROCEDURE glMatrixMode (mode: GLenum);

<*EXTERNAL glMultMatrixd:WINAPI*> 
PROCEDURE glMultMatrixd (m: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glMultMatrixf:WINAPI*> 
PROCEDURE glMultMatrixf (m: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glNewList:WINAPI*> 
PROCEDURE glNewList (list: GLuint; mode: GLenum);

<*EXTERNAL glNormal3b:WINAPI*> 
PROCEDURE glNormal3b (nx, ny, nz: GLbyte);

<*EXTERNAL glNormal3bv:WINAPI*> 
PROCEDURE glNormal3bv (v: UNTRACED REF ARRAY OF GLbyte);

<*EXTERNAL glNormal3d:WINAPI*> 
PROCEDURE glNormal3d (nx, ny, nz: GLdouble);

<*EXTERNAL glNormal3dv:WINAPI*> 
PROCEDURE glNormal3dv (v : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glNormal3f:WINAPI*> 
PROCEDURE glNormal3f (nx, ny, nz: GLfloat);

<*EXTERNAL glNormal3fv:WINAPI*> 
PROCEDURE glNormal3fv (v : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glNormal3i:WINAPI*> 
PROCEDURE glNormal3i (nx, ny, nz: GLint);

<*EXTERNAL glNormal3iv:WINAPI*> 
PROCEDURE glNormal3iv (v : UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glNormal3s:WINAPI*> 
PROCEDURE glNormal3s (nx, ny, nz: GLshort);

<*EXTERNAL glNormal3sv:WINAPI*> 
PROCEDURE glNormal3sv (v : UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glOrtho:WINAPI*> 
PROCEDURE glOrtho (left, right, bottom, top, near, far: GLdouble);

<*EXTERNAL glPassThrough:WINAPI*> 
PROCEDURE glPassThrough (token: GLfloat);

<*EXTERNAL glPixelMapfv:WINAPI*> 
PROCEDURE glPixelMapfv (map    : GLenum; 
                        mapsize: GLint; 
                        values : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glPixelMapuiv:WINAPI*> 
PROCEDURE glPixelMapuiv (map    : GLenum; 
                         mapsize: GLint; 
                         values : UNTRACED REF ARRAY OF GLuint);

<*EXTERNAL glPixelMapusv:WINAPI*> 
PROCEDURE glPixelMapusv (map    : GLenum; 
                         mapsize: GLint; 
                         values : UNTRACED REF ARRAY OF GLushort);

<*EXTERNAL glPixelStoref:WINAPI*> 
PROCEDURE glPixelStoref (pname: GLenum; param: GLfloat);

<*EXTERNAL glPixelStorei:WINAPI*> 
PROCEDURE glPixelStorei (pname: GLenum; param: GLint);

<*EXTERNAL glPixelTransferf:WINAPI*> 
PROCEDURE glPixelTransferf (pname: GLenum; param: GLfloat);

<*EXTERNAL glPixelTransferi:WINAPI*> 
PROCEDURE glPixelTransferi (pname: GLenum; param: GLint);

<*EXTERNAL glPixelZoom:WINAPI*> 
PROCEDURE glPixelZoom (xfactor: GLfloat; yfactor: GLfloat);

<*EXTERNAL glPointSize:WINAPI*> 
PROCEDURE glPointSize (size: GLfloat);

<*EXTERNAL glPolygonMode:WINAPI*> 
PROCEDURE glPolygonMode (face: GLenum; mode: GLenum);

<*EXTERNAL glPolygonStipple:WINAPI*> 
PROCEDURE glPolygonStipple (mask: UNTRACED REF ARRAY OF GLubyte);

<*EXTERNAL glPopAttrib:WINAPI*> 
PROCEDURE glPopAttrib ();

<*EXTERNAL glPopMatrix:WINAPI*> 
PROCEDURE glPopMatrix ();

<*EXTERNAL glPopName:WINAPI*> 
PROCEDURE glPopName ();

<*EXTERNAL glPushAttrib:WINAPI*> 
PROCEDURE glPushAttrib (mask: GLbitfield);

<*EXTERNAL glPushMatrix:WINAPI*> 
PROCEDURE glPushMatrix ();

<*EXTERNAL glPushName:WINAPI*> 
PROCEDURE glPushName (name: GLuint);

<*EXTERNAL glRasterPos2d:WINAPI*> 
PROCEDURE glRasterPos2d (x, y: GLdouble);

<*EXTERNAL glRasterPos2dv:WINAPI*> 
PROCEDURE glRasterPos2dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glRasterPos2f:WINAPI*> 
PROCEDURE glRasterPos2f (x, y: GLfloat);

<*EXTERNAL glRasterPos2fv:WINAPI*> 
PROCEDURE glRasterPos2fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glRasterPos2i:WINAPI*> 
PROCEDURE glRasterPos2i (x, y: GLint);

<*EXTERNAL glRasterPos2iv:WINAPI*> 
PROCEDURE glRasterPos2iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glRasterPos2s:WINAPI*> 
PROCEDURE glRasterPos2s (x, y: GLshort);

<*EXTERNAL glRasterPos2sv:WINAPI*> 
PROCEDURE glRasterPos2sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glRasterPos3d:WINAPI*> 
PROCEDURE glRasterPos3d (x, y, z: GLdouble);

<*EXTERNAL glRasterPos3dv:WINAPI*> 
PROCEDURE glRasterPos3dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glRasterPos3f:WINAPI*> 
PROCEDURE glRasterPos3f (x, y, z: GLfloat);

<*EXTERNAL glRasterPos3fv:WINAPI*> 
PROCEDURE glRasterPos3fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glRasterPos3i:WINAPI*> 
PROCEDURE glRasterPos3i (x, y, z: GLint);

<*EXTERNAL glRasterPos3iv:WINAPI*> 
PROCEDURE glRasterPos3iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glRasterPos3s:WINAPI*> 
PROCEDURE glRasterPos3s (x, y, z: GLshort);

<*EXTERNAL glRasterPos3sv:WINAPI*> 
PROCEDURE glRasterPos3sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glRasterPos4d:WINAPI*> 
PROCEDURE glRasterPos4d (x, y, z, w: GLdouble);

<*EXTERNAL glRasterPos4dv:WINAPI*> 
PROCEDURE glRasterPos4dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glRasterPos4f:WINAPI*> 
PROCEDURE glRasterPos4f (x, y, z, w: GLfloat);

<*EXTERNAL glRasterPos4fv:WINAPI*> 
PROCEDURE glRasterPos4fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glRasterPos4i:WINAPI*> 
PROCEDURE glRasterPos4i (x, y, z, w: GLint);

<*EXTERNAL glRasterPos4iv:WINAPI*> 
PROCEDURE glRasterPos4iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glRasterPos4s:WINAPI*> 
PROCEDURE glRasterPos4s (x, y, z, w: GLshort);

<*EXTERNAL glRasterPos4sv:WINAPI*> 
PROCEDURE glRasterPos4sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glReadBuffer:WINAPI*> 
PROCEDURE glReadBuffer (mode : GLenum);

<*EXTERNAL glReadPixels:WINAPI*> 
PROCEDURE glReadPixels (x     : GLint;
                        y     : GLint; 
                        width : GLsizei; 
                        height: GLsizei; 
                        format: GLenum;
                        type  : GLenum; 
                        pixels: GLvoidStar);

<*EXTERNAL glRectd:WINAPI*> 
PROCEDURE glRectd (x1, y1, x2, y2: GLdouble);

<*EXTERNAL glRectdv:WINAPI*> 
PROCEDURE glRectdv (v1, v2: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glRectf:WINAPI*> 
PROCEDURE glRectf (x1, y1, x2, y2: GLfloat);

<*EXTERNAL glRectfv:WINAPI*> 
PROCEDURE glRectfv (v1, v2: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glRecti:WINAPI*> 
PROCEDURE glRecti (x1, y1, x2, y2: GLint);

<*EXTERNAL glRectiv:WINAPI*> 
PROCEDURE glRectiv (v1, v2: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glRects:WINAPI*> 
PROCEDURE glRects (x1, y1, x2, y2: GLshort);

<*EXTERNAL glRectsv:WINAPI*> 
PROCEDURE glRectsv (v1, v2: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glRenderMode:WINAPI*> 
PROCEDURE glRenderMode (mode: GLenum): GLint;

<*EXTERNAL glRotated:WINAPI*> 
PROCEDURE glRotated (angle, x, y, z: GLdouble);

<*EXTERNAL glRotatef:WINAPI*> 
PROCEDURE glRotatef (angle, x, y, z : GLfloat);

<*EXTERNAL glScaled:WINAPI*> 
PROCEDURE glScaled (x, y, z: GLdouble);

<*EXTERNAL glScalef:WINAPI*> 
PROCEDURE glScalef (x, y, z : GLfloat);

<*EXTERNAL glScissor:WINAPI*> 
PROCEDURE glScissor (x, y: GLint; width, height: GLsizei);

<*EXTERNAL glSelectBuffer:WINAPI*> 
PROCEDURE glSelectBuffer (size: GLsizei; (*OUT*) buffer: UNTRACED REF GLuint);

<*EXTERNAL glShadeModel:WINAPI*> 
PROCEDURE glShadeModel (mode : GLenum);

<*EXTERNAL glStencilFunc:WINAPI*> 
PROCEDURE glStencilFunc (func: GLenum; ref: GLint; mask: GLuint);

<*EXTERNAL glStencilMask:WINAPI*> 
PROCEDURE glStencilMask (mask: GLuint);

<*EXTERNAL glStencilOp:WINAPI*> 
PROCEDURE glStencilOp (fail, zfail, zpass: GLenum);

<*EXTERNAL glTexCoord1d:WINAPI*>
PROCEDURE glTexCoord1d (s: GLdouble);

<*EXTERNAL glTexCoord1dv:WINAPI*>
PROCEDURE glTexCoord1dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glTexCoord1f:WINAPI*>
PROCEDURE glTexCoord1f (s: GLfloat);

<*EXTERNAL glTexCoord1fv:WINAPI*>
PROCEDURE glTexCoord1fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexCoord1i:WINAPI*>
PROCEDURE glTexCoord1i (s: GLint);

<*EXTERNAL glTexCoord1iv:WINAPI*>
PROCEDURE glTexCoord1iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexCoord1s:WINAPI*>
PROCEDURE glTexCoord1s (s: GLshort);

<*EXTERNAL glTexCoord1sv:WINAPI*>
PROCEDURE glTexCoord1sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glTexCoord2d:WINAPI*>
PROCEDURE glTexCoord2d (s, t: GLdouble);

<*EXTERNAL glTexCoord2dv:WINAPI*>
PROCEDURE glTexCoord2dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glTexCoord2f:WINAPI*>
PROCEDURE glTexCoord2f (s, t: GLfloat);

<*EXTERNAL glTexCoord2fv:WINAPI*>
PROCEDURE glTexCoord2fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexCoord2i:WINAPI*>
PROCEDURE glTexCoord2i (s, t: GLint);

<*EXTERNAL glTexCoord2iv:WINAPI*>
PROCEDURE glTexCoord2iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexCoord2s:WINAPI*>
PROCEDURE glTexCoord2s (s, t: GLshort);

<*EXTERNAL glTexCoord2sv:WINAPI*>
PROCEDURE glTexCoord2sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glTexCoord3d:WINAPI*>
PROCEDURE glTexCoord3d (s, t, r: GLdouble);

<*EXTERNAL glTexCoord3dv:WINAPI*>
PROCEDURE glTexCoord3dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glTexCoord3f:WINAPI*>
PROCEDURE glTexCoord3f (s, t, r: GLfloat);

<*EXTERNAL glTexCoord3fv:WINAPI*>
PROCEDURE glTexCoord3fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexCoord3i:WINAPI*>
PROCEDURE glTexCoord3i (s, t, r: GLint);

<*EXTERNAL glTexCoord3iv:WINAPI*>
PROCEDURE glTexCoord3iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexCoord3s:WINAPI*>
PROCEDURE glTexCoord3s (s, t, r: GLshort);

<*EXTERNAL glTexCoord3sv:WINAPI*>
PROCEDURE glTexCoord3sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glTexCoord4d:WINAPI*>
PROCEDURE glTexCoord4d (s, t, r, q: GLdouble);

<*EXTERNAL glTexCoord4dv:WINAPI*>
PROCEDURE glTexCoord4dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glTexCoord4f:WINAPI*>
PROCEDURE glTexCoord4f (s, t, r, q: GLfloat);

<*EXTERNAL glTexCoord4fv:WINAPI*>
PROCEDURE glTexCoord4fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexCoord4i:WINAPI*>
PROCEDURE glTexCoord4i (s, t, r, q: GLint);

<*EXTERNAL glTexCoord4iv:WINAPI*>
PROCEDURE glTexCoord4iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexCoord4s:WINAPI*>
PROCEDURE glTexCoord4s (s, t, r, q: GLshort);

<*EXTERNAL glTexCoord4sv:WINAPI*>
PROCEDURE glTexCoord4sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glTexEnvf:WINAPI*> 
PROCEDURE glTexEnvf (target: GLenum; 
                     pname : GLenum;
                     param : GLfloat);

<*EXTERNAL glTexEnvfv:WINAPI*> 
PROCEDURE glTexEnvfv (target: GLenum; 
                      pname : GLenum; 
                      params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexEnvi:WINAPI*> 
PROCEDURE glTexEnvi (target: GLenum; 
                     pname : GLenum;
                     param : GLint);

<*EXTERNAL glTexEnviv:WINAPI*> 
PROCEDURE glTexEnviv (target: GLenum; 
                      pname : GLenum; 
                      params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexGend:WINAPI*> 
PROCEDURE glTexGend (coord: GLenum; pname: GLenum; param: GLdouble);

<*EXTERNAL glTexGendv:WINAPI*> 
PROCEDURE glTexGendv (coord : GLenum;
                      pname : GLenum;
                      params: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glTexGenf:WINAPI*> 
PROCEDURE glTexGenf (coord: GLenum; pname: GLenum; param: GLfloat);

<*EXTERNAL glTexGenfv:WINAPI*> 
PROCEDURE glTexGenfv (coord : GLenum;
                      pname : GLenum;
                      params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexGeni:WINAPI*> 
PROCEDURE glTexGeni (coord: GLenum; pname: GLenum; param: GLint);

<*EXTERNAL glTexGeniv:WINAPI*> 
PROCEDURE glTexGeniv (coord : GLenum;
                      pname : GLenum;
                      params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTexImage1D:WINAPI*> 
PROCEDURE glTexImage1D (target    : GLenum; 
                        level     : GLint; 
                        components: GLint; 
                        width     : GLsizei; 
                        border    : GLint; 
                        format    : GLenum; 
                        type      : GLenum; 
                        pixels    : GLvoidStar);

<*EXTERNAL glTexImage2D:WINAPI*> 
PROCEDURE glTexImage2D (taget             : GLenum;
                        level, components : GLint;
                        width, height     : GLsizei;
                        border            : GLint;
                        format, type      : GLenum; 
                        pixels            : GLvoidStar);

<*EXTERNAL glTexParameterf:WINAPI*> 
PROCEDURE glTexParameterf (target: GLenum; 
                           pname : GLenum;
                           param : GLfloat);

<*EXTERNAL glTexParameterfv:WINAPI*> 
PROCEDURE glTexParameterfv (target: GLenum; 
                            pname : GLenum; 
                            params: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glTexParameteri:WINAPI*> 
PROCEDURE glTexParameteri (target: GLenum; 
                           pname : GLenum;
                           param : GLint);

<*EXTERNAL glTexParameteriv:WINAPI*> 
PROCEDURE glTexParameteriv (target: GLenum; 
                            pname : GLenum; 
                            params: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glTranslated:WINAPI*> 
PROCEDURE glTranslated (x, y, z: GLdouble);

<*EXTERNAL glTranslatef:WINAPI*> 
PROCEDURE glTranslatef (x, y, z : GLfloat);

<*EXTERNAL glVertex2d:WINAPI*> 
PROCEDURE glVertex2d (x, y: GLdouble);

<*EXTERNAL glVertex2dv:WINAPI*> 
PROCEDURE glVertex2dv (v : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glVertex2f:WINAPI*> 
PROCEDURE glVertex2f (x, y: GLfloat);

<*EXTERNAL glVertex2fv:WINAPI*> 
PROCEDURE glVertex2fv (v : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glVertex2i:WINAPI*> 
PROCEDURE glVertex2i (x, y: GLint);

<*EXTERNAL glVertex2iv:WINAPI*> 
PROCEDURE glVertex2iv (v : UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glVertex2s:WINAPI*> 
PROCEDURE glVertex2s (x, y: GLshort);

<*EXTERNAL glVertex2sv:WINAPI*> 
PROCEDURE glVertex2sv (v : UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glVertex3d:WINAPI*> 
PROCEDURE glVertex3d (x, y, z: GLdouble);

<*EXTERNAL glVertex3dv:WINAPI*> 
PROCEDURE glVertex3dv (v : UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glVertex3f:WINAPI*> 
PROCEDURE glVertex3f (x, y, z: GLfloat);

<*EXTERNAL glVertex3fv:WINAPI*> 
PROCEDURE glVertex3fv (v : UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glVertex3i:WINAPI*> 
PROCEDURE glVertex3i (x, y, z: GLint);

<*EXTERNAL glVertex3iv:WINAPI*> 
PROCEDURE glVertex3iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glVertex3s:WINAPI*> 
PROCEDURE glVertex3s (x, y, z: GLshort);

<*EXTERNAL glVertex3sv:WINAPI*> 
PROCEDURE glVertex3sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glVertex4d:WINAPI*> 
PROCEDURE glVertex4d (x, y, z, w: GLdouble);

<*EXTERNAL glVertex4dv:WINAPI*> 
PROCEDURE glVertex4dv (v: UNTRACED REF ARRAY OF GLdouble);

<*EXTERNAL glVertex4f:WINAPI*> 
PROCEDURE glVertex4f (x, y, z, w: GLfloat);

<*EXTERNAL glVertex4fv:WINAPI*> 
PROCEDURE glVertex4fv (v: UNTRACED REF ARRAY OF GLfloat);

<*EXTERNAL glVertex4i:WINAPI*> 
PROCEDURE glVertex4i (x, y, z, w: GLint);

<*EXTERNAL glVertex4iv:WINAPI*> 
PROCEDURE glVertex4iv (v: UNTRACED REF ARRAY OF GLint);

<*EXTERNAL glVertex4s:WINAPI*> 
PROCEDURE glVertex4s (x, y, z, w: GLshort);

<*EXTERNAL glVertex4sv:WINAPI*> 
PROCEDURE glVertex4sv (v: UNTRACED REF ARRAY OF GLshort);

<*EXTERNAL glViewport:WINAPI*> 
PROCEDURE glViewport (x, y : GLint; width, height : GLsizei);

END GL.
