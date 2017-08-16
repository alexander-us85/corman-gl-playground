;; Corman Lisp 3.02  (Patch level 0)
;; Copyright © Corman Technologies Inc. See LICENSE.txt for license information.
;; User: zlove.

;;; GLFW ffi declarations
#! (:library "glfw3.dll")

#define GLFW_CONTEXT_VERSION_MAJOR 0x00022002
#define GLFW_CONTEXT_VERSION_MINOR 0x00022003
#define GLFW_OPENGL_PROFILE        0x00022008
#define GLFW_OPENGL_CORE_PROFILE   0x00032001
#define GLFW_KEY_ESCAPE            256
#define GLFW_PRESS                 1

#define GL_COLOR_BUFFER_BIT 0x00004000
#define GL_ARRAY_BUFFER     0x8892
#define GL_STATIC_DRAW      0x88e4
#define GL_FLOAT            0x1406
#define GL_FALSE            0x0
#define GL_TRIANGLES        0x0004
#define GL_FRAGMENT_SHADER  0x8b30
#define GL_VERTEX_SHADER    0x8b31
#define GL_COMPILE_STATUS   0x8b81

typedef struct GLFWwindow GLFWwindow;
typedef struct GLFWmonitor GLFWmonitor;
typedef void *GLFWframebuffersizefun;

int glfwInit(void);
void glfwTerminate(void);
void glfwWindowHint(int hint, int value);
GLFWwindow* glfwCreateWindow(int width, int height, const char* title, GLFWmonitor* monitor, GLFWwindow* share);
void glfwMakeContextCurrent(GLFWwindow* window);
void* glfwGetProcAddress(const char* procname);
void glfwPollEvents(void);
void glfwSwapBuffers(GLFWwindow* window);
int glfwWindowShouldClose(GLFWwindow* window);
GLFWframebuffersizefun glfwSetFramebufferSizeCallback(GLFWwindow* window, GLFWframebuffersizefun cbfun);
int glfwGetKey(GLFWwindow* window, int key);
void glfwSetWindowShouldClose(GLFWwindow* window, int value);
!#

(DEFPARAMETER *gl-ptr* nil)

(DEFPARAMETER *gl-funcs*
    '(("glViewport"   ((x :long) (y :long) (width :long) (height :long)) :return-type :void)
      ("glClearColor" ((r :single-float) (g :single-float) (b :single-float) (a :single-float)) :return-type :void)
      ("glClear"      ((col :long)) :return-type :void)
      ("glBindBuffer" ((target :long) (buffer :long)) :return-type :void)
      ("glGenBuffers" ((n :long) (buffers (:long *))) :return-type :void)
      ("glBufferData" ((target :long) (size :long) (data (:long *)) (usage :long)) :return-type :void)
      ("glCreateShader" ((shader-type :long)) :return-type :long)
      ("glShaderSource" ((shader :long) (count :long) (string :char *) (length :long *)) :return-type :void)
      ("glCompileShader" ((shader :unsigned-long)) :return-type :void)
      ("glCreateProgram" ((:void)) :return-type :long)
      ("glAttachShader" ((program :long) (shader :long)) :return-type :void)
      ("glLinkProgram" ((program :long)) :return-type :void)
      ("glUseProgram" ((program :long)) :return-type :void)
      ("glDeleteShader" ((shader :long)) :return-type :void)
      ("glVertexAttribPointer" ((index :long) (size :long) (type :long) (normalized :long-bool) (stride :long) (pointer (:void *))) :return-type :void)
      ("glEnableVertexAttribArray" ((index :long)) :return-type :void)
      ("glGenVertexArrays" ((n :long) (arrays (:long *))) :return-type :void)
      ("glBindVertexArray" ((array :long)) :return-type :void)
      ("glDrawArrays" ((mode :long) (first :long) (count :long)) :return-type :void)
      ("glCreateShader" ((type :long)) :return-type :unsigned-long)
      ("glShaderSource" ((shader :long) (count :long) (str (:char *)) (len (:long *))) :return-type :void)
      ("glCompileShader" ((shader :unsigned-long)) :return-type :void)
      ("glGetShaderiv" ((shader :long) (pname :long) (params (:long *))) :return-type :void)
      ("glGetShaderInfoLog" ((shader :long) (maxlength :long) (length (:long *)) (log (:char *))) :return-type :void)
      ("glDeleteShader" ((shader :long)) :return-type :void)))

(DEFPARAMETER *gl-pointer-func-options* '(:linkage-type :pascal))

(DEFUN populate ()   
   (dolist (item *gl-funcs*)
        (let* ((fname (intern (car item)))
               (val `(ct:defun-pointer ,fname ,@(cdr item) ,@*gl-pointer-func-options*)))
            (setf (gethash fname *gl-ptr*) (glfwGetProcAddress (string (car item))))
            (eval val))))

(eval-when (:execute)            
(DEFMACRO gl (gl-function &rest args)
    `(if (eql *gl-ptr* nil) (error "No current context or gl table is not populated")
        (let ((func-ptr (gethash ',gl-function *gl-ptr*)))
            (if (eql func-ptr nil) (error "Function address is not found ~a" gl-function)
              (,gl-function func-ptr ,@args)))))
)
(ct::DEFUN-C-CALLBACK viewport-size-callback ((win :long) (width :long) (height :long))
    (declare (ignore win))
    (gl |glViewport| 0 0 width height)
    t)

;;; --------------------------------- RENDER LOOP DATA ---------------------------------
(DEFPARAMETER *vertices* '(-0.5 -0.5  0.0
                            0.5 -0.5  0.0
                            0.0  0.5  0.0))
;;; ------------------------------------------------------------------------------------

(DEFMACRO generate-vertex-array (vertices)
    (let ((len (gensym))
          (va  (gensym))
          (c-type `(:single-float ,(length (eval vertices)))))
        `(let ((,len ,(length (eval vertices))))
            (let ((,va (ct:malloc (ct:sizeof ',c-type))))
                (loop for i from 0 to (- ,len 1) do
                    (setf (ct:cref ,c-type ,va i) (nth i ',(eval vertices))))
                (values ,va ',c-type)))))

(DEFUN file-string (path)
    (with-open-file (stream path)
        (let ((data (make-string (file-length stream))))
            (read-sequence data stream)
            data)))

(defmacro free-objects (objs)
    (let ((items (loop for o in objs collect `(ct:free ,o))))
        `(progn ,@items)))

(DEFUN do-me-a-shader (vertex-shader-path fragment-shader-path)
  (let ((vertex-src   (ct:lisp-string-to-c-string (file-string vertex-shader-path)))
        (fragment-src (ct:lisp-string-to-c-string (file-string fragment-shader-path)))
        (shader-log   (ct:malloc (ct:sizeof '(:char 512))))
        (vertex-obj   (ct:malloc (ct:sizeof :unsigned-long)))
        (fragment-obj (ct:malloc (ct:sizeof :unsigned-long)))
        (success      (ct:malloc (ct:sizeof :long))))
        ;-------------------------------------------------------
        (setf vertex-src-*   (ct:malloc (ct:sizeof '(:long 1))))
        (setf fragment-src-* (ct:malloc (ct:sizeof '(:long 1))))
        (setf (ct:cref (:long 1) vertex-src-*   0) (ct:foreign-ptr-to-int vertex-src))
        (setf (ct:cref (:long 1) fragment-src-* 0) (ct:foreign-ptr-to-int fragment-src))
        ;-------------------------------------------------------
        (setf (ct:cref (:unsigned-long *) vertex-obj 0) (gl |glCreateShader| GL_VERTEX_SHADER))
        (gl |glShaderSource| (ct:cref (:unsigned-long *) vertex-obj 0) 1 vertex-src-* NULL)
        (gl |glCompileShader| (ct:cref (:unsigned-long *) vertex-obj 0))
        (gl |glGetShaderiv| (ct:cref (:unsigned-long *) vertex-obj 0) GL_COMPILE_STATUS success)
        (if (not (equal (ct:cref (:long 1) success 0) 1))
            (progn (gl |glGetShaderInfoLog| (ct:cref (:unsigned-long *) vertex-obj 0) 512 NULL shader-log)
                   (let ((log (ct:c-string-to-lisp-string shader-log)))
                       (format t "VERTEX-SHADER-ERROR:~% ~a~%" log))))
        ;-------------------------------------------------------
        (setf (ct:cref (:unsigned-long *) fragment-obj 0) (gl |glCreateShader| GL_FRAGMENT_SHADER))
        (gl |glShaderSource| (ct:cref (:unsigned-long *) fragment-obj 0) 1 fragment-src-* NULL)
        (gl |glCompileShader| (ct:cref (:unsigned-long *) fragment-obj 0))
        (gl |glGetShaderiv| (ct:cref (:unsigned-long *) fragment-obj 0) GL_COMPILE_STATUS success)
        (if (not (equal (ct:cref (:long 1) success 0) 1))
            (progn (gl |glGetShaderInfoLog| (ct:cref (:unsigned-long *) fragment-obj 0) 512 NULL shader-log)
                   (let ((log (ct:c-string-to-lisp-string shader-log)))
                       (format t "FRAGMENT-SHADER-ERROR:~% ~a~%" log))))
        ;-------------------------------------------------------
        (gl |glDeleteShader| (ct:cref (:unsigned-long *) fragment-obj 0))
        (gl |glDeleteShader| (ct:cref (:unsigned-long *) fragment-obj 0))
        
        (free-objects (vertex-src fragment-src vertex-src-* fragment-src-* 
                       shader-log vertex-obj   fragment-obj success))))
        
            
      
      

(DEFUN render-loop (window)
    (let ((VBO (ct:malloc (ct:sizeof :long)))
          (VAO (ct:malloc (ct:sizeof :long))))
                (gl |glGenVertexArrays| 1 VAO)
                (gl |glGenBuffers|      1 VBO)
                (gl |glBindVertexArray| (ct:cref (:long *) VAO 0))
                (gl |glBindBuffer|      GL_ARRAY_BUFFER (ct:cref (:long *) VBO 0))
                (multiple-value-bind (vertices v-type) (generate-vertex-array *vertices*)
                    (progn (gl |glBufferData|              GL_ARRAY_BUFFER (ct:sizeof v-type) vertices GL_STATIC_DRAW)
                           (gl |glVertexAttribPointer|     0 3 GL_FLOAT GL_FALSE (* 3 (ct:sizeof :single-float)) NULL)
                           (gl |glEnableVertexAttribArray| 0)
                           (loop while (not (equal (glfwWindowShouldClose window) 1)) do
                                (progn
                                    (process-input window)
                                    (gl |glViewport|   0 0 300 300)
                                    (gl |glClearColor| 1.0 0.7 0.2 1.0)
                                    (gl |glClear|      GL_COLOR_BUFFER_BIT)                           
                                    (gl |glDrawArrays| GL_TRIANGLES 0 3)
                                    (glfwSwapBuffers window)
                                    (glfwPollEvents))))
                    (ct:free VBO)
                    (ct:free VAO)
                    (ct:free vertices))))

(DEFUN process-input (window)
    (if (equal (glfwGetKey window GLFW_KEY_ESCAPE) GLFW_PRESS)
        (glfwSetWindowShouldClose window 1)))
            
(DEFUN glfwTest ()
    (progn  
        (setf *gl-ptr* (make-hash-table))      
        (if (not (glfwInit)) (error "GLFW failed to initialize"))
        (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
        (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)
        (glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
        (let ((window (glfwCreateWindow 300 300 "Sample" null null)))
            (if (not window)
                (progn (error "Sorry, we're failed to create window")
                       (glfwTerminate)
                       (return-from glfwTest 'fucked-up))
                (progn                
                    (glfwMakeContextCurrent window)
                    (populate)
                    (glfwSetFramebufferSizeCallback window (ct:get-callback-procinst 'viewport-size-callback))                                                                                
                    (render-loop window)))
        (glfwTerminate))))