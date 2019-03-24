#include <errno.h>
#include <stdlib.h>
#include <assert.h>
#include "verpool.h"

int
verpool_init(struct verpool * pool, unsigned size)
{
	pool->size = size;
	pool->num_ver = 0;
	pool->num_pending = 0;
	pool->ver = malloc(sizeof(struct vertex) * size);
	pool->dirty = 1;

	return pool->ver ? 0 : -errno;
}

int
verpool_add(struct verpool * pool, float x, float y, float color[3])
{
	int num = pool->size;
	int num_req = pool->num_ver + pool->num_pending;
	
	while(num <= num_req ){
		num *= 2;
	}
	if (num != pool->size) {
		pool->ver = realloc(pool->ver, sizeof(struct vertex) * num);
		pool->size = num;
	}

	if (!pool->ver) {
		pool->size = 0;
		pool->num_ver = 0;
		pool->num_pending = 0;
		return -errno;
	}

	pool->ver[pool->num_pending + pool->num_ver] =
		(struct vertex) { .x = x,
						  .y = y,
						  .r = color[0],
						  .g = color[1],
						  .b = color[2] };
	pool->num_pending ++;
	return 0;
}

void
verpool_remove(struct verpool * pool, int num)
{
	assert(pool->num_pending == 0);
	if (pool->num_ver > 0)
		pool->num_ver --;
}

int
verpool_update(struct verpool * pool)
{
	int rc;
	pool->num_ver += pool->num_pending;
	if (pool->num_ver < 0) {
		pool->num_ver = 0;
	}
	rc = pool->num_pending ? 1 : 0;
	pool->num_pending = 0;
	assert(pool->num_ver <= pool->size);
	return rc;
}
