/* Copyright (C) 2019 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef LOCAL_SCOPE_H_
#define LOCAL_SCOPE_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define DEQUE_CAPACITY_BITS   8
#define MAX_IDX               ((((uint64_t)1) << DEQUE_CAPACITY_BITS) - 1)
#define UNPACK_TAG(topval)    ((topval) >> DEQUE_CAPACITY_BITS)
#define UNPACK_IDX(topval)    ((topval) & MAX_IDX)
#define PACK_TAGIDX(tag, idx) (((tag) << DEQUE_CAPACITY_BITS) | (idx))
#define TOPBIT64              (((uint64_t)1) << 63)

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

uint64_t lockLocalScope(GC_state s);
void unlockLocalScope(GC_state s, uint64_t oldval);

uint32_t pollCurrentLocalScope(GC_state s);

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

#endif /* LOCAL_SCOPE_H_ */
