#include "canvas.h"

static const char* vertex_shader_text =
	"#version 110\n"
	"uniform mat4 MVP;\n"
	"attribute vec3 vCol;\n"
	"attribute vec2 vPos;\n"
	"varying vec3 color;\n"
	"void main()\n"
	"{\n"
	"	 gl_Position = MVP * vec4(vPos, 0.0, 1.0);\n"
	"	 color = vCol;\n"
	"}\n";

static const char* fragment_shader_text =
	"#version 110\n"
	"varying vec3 color;\n"
	"void main()\n"
	"{\n"
	"	 gl_FragColor = vec4(color, 1.0);\n"
	"}\n";


static int
drawitem_init(struct drawitem * item)
{
	int rc = verpool_init(&item->vpool, 64 * 1024);
	if (rc) {
		return rc;
	}

	glGenBuffers(1, &item->vbuf);
	glBindBuffer(GL_ARRAY_BUFFER, item->vbuf);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
			sizeof(struct vertex), (void*) 0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE,
			sizeof(struct vertex), (void*) (sizeof(float) * 2));

	return 0;
}

static struct glsl_shader *
canvas_shader_init(const char * vertex_shader_text,
		const char * fragment_shader_text)
{
	struct glsl_shader * shader = calloc(1, sizeof(*shader));
	if (!shader) {
		return NULL;
	}

	GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vertex_shader, 1, &vertex_shader_text, NULL);
	glCompileShader(vertex_shader);

	GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fragment_shader, 1, &fragment_shader_text, NULL);
	glCompileShader(fragment_shader);

	shader->program = glCreateProgram();

	glAttachShader(shader->program, vertex_shader);
	glAttachShader(shader->program, fragment_shader);
	glLinkProgram(shader->program);

	shader->mvp_location = glGetUniformLocation(shader->program, "MVP");
	shader->vpos_location = glGetAttribLocation(shader->program, "vPos");
	shader->vcol_location = glGetAttribLocation(shader->program, "vCol");

	return shader;
}

static int
canvas_update(struct canvas_ctx * ctx)
{
	int rc = 0;
	canvas_lock(ctx);
	for (int i=0; i < DRAW_LAST; i++) {
		struct drawitem * draw = &ctx->draws[i];
		if (verpool_update(&draw->vpool)) {
			rc ++;
			glBindBuffer(GL_ARRAY_BUFFER, draw->vbuf);
			glBufferData(GL_ARRAY_BUFFER, draw->vpool.num_ver * sizeof(struct vertex),
					draw->vpool.ver, GL_STATIC_DRAW);
		}
	}
	canvas_unlock(ctx);

	return rc;
}

void
canvas_render(struct canvas_ctx * ctx)
{
	static GLenum modes[] = { GL_POINTS, GL_LINES, GL_TRIANGLES };
	float ratio;
	mat4x4 m, p, mvp;

	glfwGetFramebufferSize(ctx->win, &ctx->cur_state.win_size[0], &ctx->cur_state.win_size[1]);

	ratio = (float)ctx->cur_state.win_size[0] / (float) ctx->cur_state.win_size[1];

	int w = ctx->cur_state.canvas_size[0];
	int h = ctx->cur_state.canvas_size[1];

	mat4x4_identity(m);
	//mat4x4_rotate_Z(m, m, (float) glfwGetTime());
	mat4x4_ortho(p, -ratio * h, ratio * h, -h, h, 1.f, -1.f);
	mat4x4_mul(mvp, p, m);

	glViewport(0, 0, ctx->cur_state.win_size[0],  ctx->cur_state.win_size[1]);

	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	glDisable(GL_DEPTH_TEST);

	glUseProgram(ctx->cur_state.shader->program);
	glUniformMatrix4fv(ctx->cur_state.shader->mvp_location,
			1, GL_FALSE, (const GLfloat*) mvp);
	glPointSize(ctx->cur_state.point_size);
	for (int i=0; i < DRAW_LAST; i++) {
		if (ctx->draws[i].vpool.num_ver) {
			glBindBuffer(GL_ARRAY_BUFFER, ctx->draws[i].vbuf);
			//glEnableVertexAttribArray(0);
			//glEnableVertexAttribArray(1);
			glEnableVertexAttribArray(ctx->cur_state.shader->vpos_location);
			glVertexAttribPointer(ctx->cur_state.shader->vpos_location, 2, GL_FLOAT, GL_FALSE,
					sizeof(struct vertex), (void*) 0);
			glEnableVertexAttribArray(ctx->cur_state.shader->vcol_location);
			glVertexAttribPointer(ctx->cur_state.shader->vcol_location, 3, GL_FLOAT, GL_FALSE,
					sizeof(struct vertex), (void*) (sizeof(float) * 2));
			int err = (int)glGetError();
			glDrawArrays(modes[i], 0, ctx->draws[i].vpool.num_ver/(i+1));
		}
	}
}

void canvas_key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);

static void
canvas_repaint(GLFWwindow * win)
{
	struct canvas_ctx * ctx = glfwGetWindowUserPointer(win);
	assert(ctx);

	canvas_update(ctx);

	canvas_render(ctx);

	glfwSwapBuffers(win);
}


void *
canvas_thread(void * arg)
{
	int i, rc;
	struct canvas_ctx * ctx = arg;
	GLuint vertex_buffer, vertex_shader, fragment_shader, program;
	GLint mvp_location, vpos_location, vcol_location;

	ctx->win = glfwCreateWindow(ctx->settings.win_size[0],
			ctx->settings.win_size[1], "lisp canvas", NULL, NULL);
	if (!ctx->win) {
		return NULL;
	}

	glfwSetWindowUserPointer(ctx->win, ctx);
	glfwSetKeyCallback(ctx->win, canvas_key_callback);
	glfwSetWindowRefreshCallback(ctx->win, canvas_repaint);

	glfwMakeContextCurrent(ctx->win);
	gladLoadGLLoader((GLADloadproc) glfwGetProcAddress);
	glfwSwapInterval(1);

	ctx->cur_state.shader = canvas_shader_init(vertex_shader_text,
			fragment_shader_text );

	for(i=0;i < DRAW_LAST; i++) {
		drawitem_init(&ctx->draws[i]);
	}

	rc = pthread_mutex_lock(&ctx->mutex);
	assert(rc == 0);
	ctx->status = RUNNING;
	rc = pthread_cond_broadcast(&ctx->cond);
	assert(rc == 0);
	rc = pthread_mutex_unlock(&ctx->mutex);

	int status = 0;
	double last = glfwGetTime();
	double delta = 0;
	while (status != -1 && !glfwWindowShouldClose(ctx->win))
	{
		//glfwPollEvents();
		//glfwWaitEvents();
		glfwWaitEventsTimeout(1.0/90.0);
		if (canvas_update(ctx)) {
			canvas_render(ctx);
			glfwSwapBuffers(ctx->win);
		}
		canvas_lock(ctx);
		status = ctx->status;
		canvas_unlock(ctx);
	}

	glfwDestroyWindow(ctx->win);

	glfwTerminate();
	return NULL;
}
