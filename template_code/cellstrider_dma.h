/*
 * CellStrider Project
 *
 * dmalist_spe.h
 *
 * Jae-Seung Yeom <jyeom@cs.vt.edu>
 *
 * Computer Science
 * Virginia Tech
 *
 * Sept, 2008
 *
 */

#ifndef __CELLSTRIDER_DMA_H__
#define __CELLSTRIDER_DMA_H__

#include <malloc_align.h>
#include <free_align.h>
#include <spu_mfcio.h>
#include <stdint.h>
//#include "../cellstrider_type.h"
#include "MMGP_spu.h"
#include <stdio.h>

// pulled directly in from cellstrider_type.h
#define DMA_QUANTUM_SZ		16

#define DMA_QUANTUM_MASK	0x0000000F
#define DMA_SZ_LIMIT		16384 // it is multiple of 16(DMA_QUANTUM_SZ)
#define DMA_LIST_LIMIT		2048
#define ALIGN_BY		7  // 2^(ALIGN_BY)/8 bytes
#define DMA_ALIGN		((ALIGN_BY==7) ? 128 : ((ALIGN_BY==6)? 64: ((ALIGN_BY==5)? 32: 8)))
#define ATTR_ALIGN		__attribute__ ((aligned (DMA_ALIGN)))
#define DMA_ALIGN_MASK		0x0000007F

#define IS_ALIGN_OK(_P_)	!(((uintptr_t)(_P_)) & DMA_ALIGN_MASK) 
#define IS_SIZE_OK(_S_)		!((_S_) & DMA_QUANTUM_MASK)
#define EPADDING(_S_)		((DMA_QUANTUM_SZ - ((_S_) % DMA_QUANTUM_SZ)) % DMA_QUANTUM_SZ)

typedef struct spe_dma_list
{
    volatile mfc_list_element_t* data ATTR_ALIGN;
    uint32_t length;
    uint32_t alloclength;
} spe_dma_list_t;

static inline void allocate_dma_list(spe_dma_list_t* list, uint32_t len, uint32_t num_dma_cmds_per_element)
{
    list->length = len*num_dma_cmds_per_element;
    list->alloclength = list->length;
    list->data = (mfc_list_element_t*)_malloc_align(list->length*sizeof(mfc_list_element_t), 7);
    if (list->data == NULL) {
        printf("Allocation of dma list (length=%u) failed\n", list->length);
        exit(0);
    }

    #ifdef DEBUG
    printf("SPE%d num_dma_cmds_per_element: %u   list.length: %u (%lu bytes)\n", SPE_id, num_dma_cmds_per_element, list->length, list->length*sizeof(mfc_list_element_t));
    #endif
}

static inline void free_dma_list(spe_dma_list_t* list)
{
    list->length = 0;
    list->alloclength = 0;
    _free_align(list->data);
}

#define add_single_to_dma_list(list, j, ea, sz) { \
        list->data[j].eal  = ea;                  \
        list->data[j].size = sz;                  \
        j++;                                      \
}

#define add_multi_to_dma_list(list, j, ea, sz, num_dma_cmds) {                    \
        int i_dma_cmd;                                                            \
        uint32_t rest_sz = sz;                                                    \
        uint64_t effaddr = ea;                                                    \
        for (i_dma_cmd  = 0; i_dma_cmd < ((int)num_dma_cmds - 1); i_dma_cmd ++) { \
            list->data[j].eal  = effaddr;                                         \
            list->data[j].size = DMA_SZ_LIMIT;                                    \
            effaddr += DMA_SZ_LIMIT;                                              \
            rest_sz -= DMA_SZ_LIMIT;                                              \
            j ++;                                                                 \
        }                                                                         \
        list->data[j].eal  = effaddr;                                             \
        list->data[j].size = rest_sz;                                             \
        j ++;                                                                     \
}

/*
inline void add_single_to_dma_list(spe_dma_list_t* list, uint32_t idx, uint64_t ea, uint32_t sz)
{
    list->data[idx].eal  = ea;
    list->data[idx].size = sz;
}

inline void add_multi_to_dma_list(spe_dma_list_t* list, uint32_t idx, uint64_t ea, \
    uint32_t sz, int num_dma_cmds_per_element)
{
    int i;

    for (i  = 0; i < (num_dma_cmds_per_element - 1); ++i, idx++) { 
        list->data[idx].eal  = ea;
        list->data[idx].size = DMA_SZ_LIMIT;
        ea += DMA_SZ_LIMIT;
        sz -= DMA_SZ_LIMIT;
    }
    list->data[idx].eal  = ea;
    list->data[idx].size = sz;
}
*/

void add_to_dma_list(spe_dma_list_t* list, uint32_t len, uint64_t ea, \
    uint32_t dma_sz, uint32_t stride_sz, uint32_t num_dma_cmds_per_element)
{
    uint32_t j=0, n=len;
    
    if (list->length / num_dma_cmds_per_element < n) {
        #ifdef DEBUG
        fprintf(stderr, "dma list length is too short! (%u/%u < %u) So, reallocated\n", list->length , num_dma_cmds_per_element, n);
        #endif
        if (list->alloclength / num_dma_cmds_per_element >= n) {
            list->length = n*num_dma_cmds_per_element;
        } else {
            free_dma_list(list);
            allocate_dma_list(list, len, num_dma_cmds_per_element);
        }
    }
    // FIXME: there is no guarantee that ea_base is same for all if mem size > 4GB

    if (num_dma_cmds_per_element == 1)
    {
        while(n > 16)
        {
            add_single_to_dma_list(list, j, ea, dma_sz); // 1       
            ea += stride_sz;
            //j += num_dma_cmds_per_element;

            add_single_to_dma_list(list, j, ea, dma_sz); // 2
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 3
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 4
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 5
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 6
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 7
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 8
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 9
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 10
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 11
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 12
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 13
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 14
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 15
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 16
            ea += stride_sz;

            n -= 16;
        }

        while(n > 4)
        {
            add_single_to_dma_list(list, j, ea, dma_sz); // 1
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 2
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 3
            ea += stride_sz;

            add_single_to_dma_list(list, j, ea, dma_sz); // 4
            ea += stride_sz;

            n -= 4;
        }

        while(n > 0)
        {
            add_single_to_dma_list(list, j, ea, dma_sz); // 1
            ea += stride_sz;

            --n ;
        }
    } else {
        while(n > 16)
        {
            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 1       
            ea += stride_sz;
            //j += num_dma_cmds_per_element;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 2
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 3
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 4
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 5
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 6
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 7
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 8
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 9
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 10
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 11
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 12
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 13
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 14
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 15
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 16
            ea += stride_sz;

            n -= 16;
        }

        while(n > 4)
        {
            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 1
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 2
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 3
            ea += stride_sz;

            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 4
            ea += stride_sz;

            n -= 4;
        }

        while(n > 0)
        {
            add_multi_to_dma_list(list, j, ea, dma_sz, num_dma_cmds_per_element); // 1
            ea += stride_sz;

            --n ;
        }
    }
}

inline void DMA_getl(volatile void* buff, uint64_t ea_base, spe_dma_list_t* list, int tag, \
    uint32_t num_dma_cmds_per_element, uint32_t alloc_unit_sz)
{
    if (list->length > DMA_LIST_LIMIT) {
        uint32_t i = 0;
        uint32_t full_size = DMA_LIST_LIMIT / num_dma_cmds_per_element * num_dma_cmds_per_element; 
        uint32_t num_lists = list->length / full_size;
        uint32_t rest_list_size = list->length % full_size;
        uint32_t sz_list = alloc_unit_sz * full_size / num_dma_cmds_per_element;
        volatile mfc_list_element_t* L = list->data;

        for(i = 0 ; i < num_lists ; i ++) {
            mfc_getl(buff, ea_base, L, full_size*sizeof(mfc_list_element_t), tag, 0, 0); 
            buff += sz_list;
            L += full_size;
            ea_base += sz_list;
        }
        if (rest_list_size) {
            mfc_getl(buff, ea_base, L, rest_list_size*sizeof(mfc_list_element_t), tag, 0, 0); 
        }
        // FIXME: there is no guarantee that ea_base is same for all if mem size > 4GB
    } else {
        mfc_getl(buff, ea_base, list->data, list->length*sizeof(mfc_list_element_t), tag, 0, 0); 
    }
}

inline void DMA_putl(volatile void* buff, uint64_t ea_base, spe_dma_list_t* list, int tag, \
    uint32_t num_dma_cmds_per_element, uint32_t alloc_unit_sz)
{
    if (list->length > DMA_LIST_LIMIT) {
        uint32_t i = 0;
        uint32_t full_size = DMA_LIST_LIMIT / num_dma_cmds_per_element * num_dma_cmds_per_element; 
        uint32_t num_lists = list->length / full_size;
        uint32_t rest_list_size = list->length % full_size;
        uint32_t sz_list = alloc_unit_sz * full_size / num_dma_cmds_per_element;
        volatile mfc_list_element_t* L = list->data;

        for(i = 0 ; i < num_lists ; i ++) {
            mfc_putl(buff, ea_base, L, full_size*sizeof(mfc_list_element_t), tag, 0, 0); 
            buff += sz_list;
            L += full_size;
            ea_base += sz_list;
        }
        if (rest_list_size) {
            mfc_putl(buff, ea_base, L, rest_list_size*sizeof(mfc_list_element_t), tag, 0, 0); 
        }
        // FIXME: there is no guarantee that ea_base is same for all if mem size > 4GB
    } else {
        mfc_putl(buff, ea_base, list->data, list->length*sizeof(mfc_list_element_t), tag, 0, 0); 
    }
}

inline void DMA_get(volatile void* buff, uint64_t addr, unsigned int dma_sz, int tag)
{
    if (dma_sz > DMA_SZ_LIMIT) {
        unsigned int j;
        unsigned int rest = dma_sz % DMA_SZ_LIMIT;
        unsigned int num_full_dma = dma_sz / DMA_SZ_LIMIT;
        unsigned int list_len = num_full_dma + (rest ? 1: 0);

        spe_dma_list_t list;
        allocate_dma_list(&list, list_len, 1);

        for (j=0; j < num_full_dma; j++) {
            list.data[j].eal  = addr;
            list.data[j].size = DMA_SZ_LIMIT;
            addr += DMA_SZ_LIMIT;
        }

        if (rest) {
            list.data[j].eal  = addr;
            list.data[j].size = rest;
        }

        // One list can have total 32M but there is only 256K LS. Thus, one list is enough
        mfc_getl(buff, addr, list.data, list_len*sizeof(mfc_list_element_t), tag, 0, 0);

        free_dma_list(&list);
    } else {
        mfc_get(buff, addr, dma_sz, tag, 0, 0);
    }
}

inline void DMA_put(volatile void* buff, uint64_t addr, unsigned int dma_sz, int tag)
{
    if (dma_sz > DMA_SZ_LIMIT) {
        unsigned int j;
        unsigned int rest = dma_sz % DMA_SZ_LIMIT;
        unsigned int num_full_dma = dma_sz / DMA_SZ_LIMIT;
        unsigned int list_len = num_full_dma + (rest ? 1: 0);

        spe_dma_list_t list;
        allocate_dma_list(&list, list_len, 1);

        for (j=0; j < num_full_dma; j++) {
            list.data[j].eal  = addr;
            list.data[j].size = DMA_SZ_LIMIT;
            addr += DMA_SZ_LIMIT;
        }

        if (rest) {
            list.data[j].eal  = addr;
            list.data[j].size = rest;
        }

        // One list can have total 32M but there is only 256K LS. Thus, one list is enough
        mfc_putl(buff, addr, list.data, list_len*sizeof(mfc_list_element_t), tag, 0, 0);

        free_dma_list(&list);
    } else {
        mfc_put(buff, addr, dma_sz, tag, 0, 0);
    }
}

#endif  // __CELLSTRIDER_DMA_H__
