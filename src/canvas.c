#include "canvas.h"

#define MAX_CONVAS  16

static pthread_once_t canvas_once_ctl = PTHREAD_ONCE_INIT;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static int num_ctx = 0;
struct canvas_ctx_list __ctx_list;

void * canvas_thread(void *);
static void   canvas_cleanup(struct canvas_ctx *ctx);

int
foo(int a, int b)
{
	return a + b;
}

static void
error_callback(int error, const char* description)
{
    fprintf(stderr, "Error: %s\n", description);
}

void
canvas_key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GLFW_TRUE);
}

static void
canvas_init_once(void)
{
    glfwSetErrorCallback(error_callback);

    if (!glfwInit())
        exit(EXIT_FAILURE);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);
}

int
canvas_init(int w, int h)
{
	int rc;
	rc = pthread_once(&canvas_once_ctl, canvas_init_once);
	assert(rc == 0);

	struct canvas_ctx * ctx;
	ctx = calloc(1, sizeof(*ctx));
	if (!ctx) {
		return -errno;
	}
	ctx->status = INIT;
	ctx->settings.win_size[0] = w;
	ctx->settings.win_size[1] = h;
	rc = pthread_mutex_init(&ctx->mutex, NULL);
	assert(rc == 0);
	rc = pthread_cond_init(&ctx->cond, NULL);
	assert(rc == 0);

	rc = pthread_mutex_lock(&mutex);
	assert(rc == 0);
	__ctx_list.ctx_list[num_ctx++] = ctx;

	assert(num_ctx < MAX_CONVAS);
	rc = pthread_mutex_unlock(&mutex);
	assert(rc == 0);

	rc = pthread_create(&ctx->tid, NULL,
			canvas_thread, ctx);
	if (rc) {
		free(ctx);
		return -errno;
	}

	rc = pthread_mutex_lock(&ctx->mutex);
	assert(rc == 0);
	while(ctx->status != RUNNING) {
		rc = pthread_cond_wait(&ctx->cond, &ctx->mutex);
		assert(rc == 0);
	}
	rc = pthread_mutex_unlock(&ctx->mutex);

	return 0;
}

int
canvas_fini()
{
	GET_CTX();

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

void
canvas_active(int canvas)
{
	__ctx_list.cur = canvas;
}


static void
canvas_cleanup(struct canvas_ctx *ctx)
{
}


int
canvas_undo()
{
	return 0;
}

//! [code]
