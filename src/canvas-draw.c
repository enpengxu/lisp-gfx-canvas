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
	pool->dirty = 1;

	glGenBuffers(1, &pool->vbuf);
	glBindBuffer(GL_ARRAY_BUFFER, pool->vbuf);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
			sizeof(struct vertex), (void*) 0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE,
			sizeof(struct vertex), (void*) (sizeof(float) * 2));

	return pool->ver ? 0 : -errno;
}


static int
ver_pool_add(struct ver_pool * pool, float x, float y, float color[3])
{
	if (pool->num_ver >= pool->size)
		pool->ver = realloc(pool->ver, 2 * pool->size);

	if (!pool->ver)
		return -errno;

	pool->ver[pool->num_ver++] =
		(struct vertex) { .x = x, .y = y,
						  .r = color[0],
						  .g = color[1],
						  .b = color[2] };
	pool->dirty = 1;
	return 0;
}

static int
canvas_update(struct canvas_ctx * ctx)
{
	int rc = 0;
	for (int i=0; i < DRAW_LAST; i++) {
		struct drawitem * draw = ctx->draws[i];
		if (draw->dirty && draw->vpool.num_ver) {
			rc = 1;
			glBindBuffer(GL_ARRAY_BUFFER, draw->vbuf);
			glBufferData(GL_ARRAY_BUFFER, draw->vpool->num_ver * sizeof(struct vertex),
					draw->vpool->ver, GL_STATIC_DRAW);
			draw->dirty = 0;
		}
	}
	return rc;
}

static void
canvas_render(struct canvas_ctx * ctx)
{
	static GLenum modes[] = { GL_POINTS, GL_LINES, GL_TIRANGLES };
	float ratio;
	mat4x4 m, p, mvp;

	glfwGetFramebufferSize(ctx->win, &ctx->cur_states.win_size[0], &ctx->cur_states.win_size[1]);
	ratio = (float)ctx->cur_states.win_size[0] / (float) ctx->cur_states.win_size[1];

	w = ctx->canvas_size[0];
	h = ctx->canvas_size[1];

	mat4x4_identity(m);
	//mat4x4_rotate_Z(m, m, (float) glfwGetTime());
	mat4x4_ortho(p, -ratio * h, ratio * h, -h.f, h.f, 1.f, -1.f);
	mat4x4_mul(mvp, p, m);

	glViewport(0, 0, ctx->cur_states.win_size[0],  ctx->cur_states.win_size[1]);
	glClear(GL_COLOR_BUFFER_BIT);

	glUseProgram(program);
	glUniformMatrix4fv(mvp_location, 1, GL_FALSE, (const GLfloat*) mvp);

	for (int i=0; i < DRAW_LAST; i++) {
		if (ctx->draws[i].vpool.num_ver) {
			glDrawArray(modes[i], 0, ctx->draws[i].vpool.num_ver/(i+1));
		}
	}
}

int
canvas_draw_begin(int type)
{
	GET_CTX();

	if (ctx->cur_state.type != -1)
		return -1;

	switch(type) {
	case DRAW_POINT:
	case DRAW_LINE:
	case DRAW_TRIANGLE:
		break;
	default:
		return -1;
	}

	ctx->cur_state.type = type;
	return 0;
}

int canvas_draw_color(float r, float g, floar b)
{
	GET_CTX();
	if (ctx->cur_state.type == -1)
		return -1;

	ctx->cur_state.color[0] = r;
	ctx->cur_state.color[1] = g;
	ctx->cur_state.color[2] = b;
	return 0;
}

int canvas_draw_size(float s)
{
	GET_CTX();
	if (ctx->cur_state.type == -1)
		return -1;

	ctx->cur_state.point_size = s;
	return 0;
}

int canvas_draw_point(float x, float y)
{
	GET_CTX();
	if (ctx->cur_state.type == -1)
		return -1;

	canvas_lock(ctx);
	struct drawitem * draw = &ctx->draws[ctx->cur_state.type];
	ver_pool_add(&draw->vpool, x, y, ctx->cur_state.color);
	draw->dirty = 1;
	canvas_unlock(ctx);
}

int
canvas_draw_end()
{
	GET_CTX();
	if (ctx->type == -1)
		return -1;
	ctx->type = -1;
	return 0;
}
