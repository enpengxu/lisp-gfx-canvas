#ifndef __VPOOL_H
#define __VPOOL_H

struct vertex {
	float x, y;
	float r, g, b;
};

struct verpool {
	int dirty;
	unsigned size;    /* pool size */
	int num_ver; /* valid point num */
	int num_pending;    /* num of modified added (>0) or deleted (<0) */
	struct vertex * ver;
};

int verpool_init(struct verpool * pool, unsigned size);
int verpool_add(struct verpool * pool, float x, float y, float color[3]);
void verpool_remove(struct verpool * pool, int num);
int verpool_sync(struct verpool * pool);


#endif
