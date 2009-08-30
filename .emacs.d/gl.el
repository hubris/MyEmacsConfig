;;Coloration OpenGL
;;(insert (regexp-opt '("GLenum" "GLboolean" "GLbitfield" "GLbyte" "GLshort" "GLint" "GLsizei" "GLubyte" "GLushort" "GLuint" "GLfloat" "GLclampf" "GLdouble" "GLclampd" "GLvoid" ) 'words))
;;(insert (regexp-opt '("glBegin" "glEnd" "glPush" "glPop" "glPushAttrib"  "glPopAttrib") 'words))

(defvar font-lock-struct-face 'font-lock-struct-face
"Face name for c structures.")
(copy-face 'default 'font-lock-struct-face)
(set-face-foreground 'font-lock-struct-face "tomato1")

(setq opengl-keywords 
  '(
    ("\\<\\(GL\\(?:b\\(?:itfield\\|oolean\\|yte\\)\\|clamp[df]\\|double\\|enum\\|float\\|int\\|s\\(?:hort\\|izei\\)\\|u\\(?:byte\\|\\(?:in\\|shor\\)t\\)\\|void\\)\\)\\>" . font-lock-type-face)
    ("\\<GL_\\w+\\>" . font-lock-constant-face)
    ("\\<[A-Z_][A-Z_0-9]+\\>" . font-lock-constant-face)
    ("\\<\\(gl\\(?:Begin\\|End\\|P\\(?:op\\(?:Attrib\\)?\\|ush\\(?:Attrib\\)?\\)\\)\\)\\>" . font-lock-keyword-face)
    ("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
;    ("\\<[^0-9<]\\w+->" . font-lock-struct-face)
;    ("\\<[^0-9<]\\w+\\." . font-lock-struct-face)
))

(font-lock-add-keywords 'c-mode opengl-keywords)
(font-lock-add-keywords 'c++-mode opengl-keywords)
