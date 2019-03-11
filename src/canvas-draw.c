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


static struct draw_item _draws[DRAW_LAST] = { 0 };

static int
ver_pool_init(struct ver_pool * pool, unsigned size)
{
	pool->size = size;
	pool->num_ver = 0;
	pool->ver = malloc(sizeof(struct vertex) * size);
	return pool->ver ? 0 : -errno;
}


static int
ver_pool_add(struct ver_pool * pool, float x, float y, float color[3])
{
	if (pool->num_ver >= pool->size)
		pool->ver = realloc(pool->ver, 2 * pool->size);

	if (!pool->ver)
		return -errno;

	pool->ver[pool->num_ver++] = (struct vertex) {	.x = x, .y = y, 
													.r = color[0],
													.g = color[1],
													.b = color[2]};
	return 0;
}

static void
canvas_draw(struct canvas_ctx * ctx)
{
	float ratio;
	int w, h;
	mat4x4 m, p, mvp;

	glfwGetFramebufferSize(ctx->win, &w, &h);
	ratio = width / (float) height;

	mat4x4_identity(m);
	//mat4x4_rotate_Z(m, m, (float) glfwGetTime());
	mat4x4_ortho(p, -ratio, ratio, -1.f, 1.f, 1.f, -1.f);
	mat4x4_mul(mvp, p, m);

	glViewport(0, 0, width, height);
	glClear(GL_COLOR_BUFFER_BIT);

	glUseProgram(program);
	glUniformMatrix4fv(mvp_location, 1, GL_FALSE, (const GLfloat*) mvp);

	glDrawArrays(GL_TRIANGLES, 0, 3);
}

int
canvas_draw_begin(int type)
{
	GET_CTX();

	switch(type) {
	case DRAW_POINT:
	case DRAW_LINE:
	case DRAW_TRIANGLE:
		break;
	default:
		return -1;
	}
	if (ctx->cur_state.type != -1)
		return -1;

	ctx->cur_state.type = type;
	return 0;
}

int canvas_draw_color(float r, float g, floar b);
int canvas_draw_size(float s);
int canvas_draw_point(float x, float y);

int
canvas_draw_end()
{
	GET_CTX();
	if (ctx->type == -1)
		return -1;

	
	
	return 0;
}

int canvas_undo();
