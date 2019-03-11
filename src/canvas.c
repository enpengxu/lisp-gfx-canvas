#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "linmath.h"

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <assert.h>

#define MAX_CONVAS  16

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static int num_ctx = 0;
static struct convas_ctx * ctx_list[MAX_CONVAS];

static void * canvas_thread(void *);
static void   canvas_cleanup(struct canvas_ctx *ctx);



static void error_callback(int error, const char* description)
{
    fprintf(stderr, "Error: %s\n", description);
}

static void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GLFW_TRUE);
}

int
foo(int a, int b)
{
	return a + b;
}


int
canvas_init(int w, int h)
{
	struct canvas_ctx * ctx;
	ctx = calloc(1, sizeof(*ctx));
	if (!ctx) {
		return -errno;
	}
	int rc = pthread_create(&ctx->tid, NULL,
				canvas_thread,
				NULL);
	if (rc) {
		free(ctx);
		return -errno;
	}
	rc = pthread_mutex_lock(&mutex);
	assert(rc == 0);
	ctx_list[num_ctx++] = ctx;

	assert(num_ctx < MAX_CONVAS);
	rc = pthread_mutex_unlock(&mutex);
	assert(rc == 0);

	return 0;
}

void
canvas_fini(int canvas)
{
	if (canvas < 0 || canvas >= MAX_CONVAS) {
		canvas_log("error, invalid canvas");
		return;
	}
	CANVAS_CTX(canvas);

	int rc = pthread_mutex_lock(&ctx->mutex);
	assert(rc == 0);
	ctx->status = -1;
	rc = pthread_cond_signal(&ctx->cond);
	assert(rc == 0);
	rc = pthread_mutex_unlock(&ctx->mutex);

	pthread_join(ctx->tid, NULL);
	canvas_cleanup(ctx);
	free(ctx);
	/* TODO */

	glfwTerminate();
}

pthread_once_t canvas_once_ctl = PTHREAD_ONCE_INIT;
static void
canvas_once_init(void)
{
    glfwSetErrorCallback(error_callback);

    if (!glfwInit())
        exit(EXIT_FAILURE);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

}

static void
canvas_repaint(GLFWwindow * win)
{
	struct canvas_ctx * ctx = glfwGetWindowUserPointer(win);
	assert(ctx);
}

static void *
canvas_thread(void * arg)
{
	struct canvas_ctx * ctx = arg;
    GLuint vertex_buffer, vertex_shader, fragment_shader, program;
    GLint mvp_location, vpos_location, vcol_location;

	pthread_once(canvas_once_ctl, canvas_once_init);

    ctx->win = glfwCreateWindow(ctx->size[0], ctx->size[1], "lisp canvas", NULL, NULL);
    if (!ctx->win) {
		return NULL;
    }

	glfwGetWindowUserPointer(ctx->win, ctx);
    glfwSetKeyCallback(ctx->win, canvas_key_callback);
	glfwSetWindowRefreshCallback(ctx->win, canvas_repaint);

    glfwMakeContextCurrent(ctx->win);
    gladLoadGLLoader((GLADloadproc) glfwGetProcAddress);
    glfwSwapInterval(1);

    // NOTE: OpenGL error checks have been omitted for brevity
    glGenBuffers(1, &vertex_buffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &vertex_shader_text, NULL);
    glCompileShader(vertex_shader);

    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_shader_text, NULL);
    glCompileShader(fragment_shader);

    program = glCreateProgram();
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);

    mvp_location = glGetUniformLocation(program, "MVP");
    vpos_location = glGetAttribLocation(program, "vPos");
    vcol_location = glGetAttribLocation(program, "vCol");

    glEnableVertexAttribArray(vpos_location);
    glVertexAttribPointer(vpos_location, 2, GL_FLOAT, GL_FALSE,
                          sizeof(vertices[0]), (void*) 0);
    glEnableVertexAttribArray(vcol_location);
    glVertexAttribPointer(vcol_location, 3, GL_FLOAT, GL_FALSE,
                          sizeof(vertices[0]), (void*) (sizeof(float) * 2));

    while (!glfwWindowShouldClose(window))
    {

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwDestroyWindow(window);

    glfwTerminate();
    exit(EXIT_SUCCESS);
}

static void
canvas_cleanup(struct canvas_ctx *ctx)
{
}


//! [code]
