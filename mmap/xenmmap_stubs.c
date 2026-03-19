/*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include "mmap_stubs.h"

#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>

#define Intf_val(a) ((struct mmap_interface *)Data_abstract_val(a))
#define Wsize_bsize_round(n) (Wsize_bsize( (n) + sizeof(value) - 1 ))

value
stub_mmap_alloc (void *addr, size_t len)
{
        CAMLparam0 ();
        CAMLlocal1 (result);
        result = caml_alloc (Wsize_bsize_round
                             (sizeof (struct mmap_interface)), Abstract_tag);
        Intf_val (result)->addr = addr;
        Intf_val (result)->len = len;
        CAMLreturn (result);
}

CAMLprim value
stub_mmap_init (value fd, value pflag, value mflag, value len, value offset)
{
        CAMLparam5 (fd, pflag, mflag, len, offset);
        CAMLlocal1 (result);
        int c_pflag, c_mflag;
        void *addr;
        size_t length;

        switch (Int_val (pflag))
          {
          case 0:
                  c_pflag = PROT_READ;
                  break;
          case 1:
                  c_pflag = PROT_WRITE;
                  break;
          case 2:
                  c_pflag = PROT_READ | PROT_WRITE;
                  break;
          default:
                  caml_invalid_argument ("protectiontype");
          }

        switch (Int_val (mflag))
          {
          case 0:
                  c_mflag = MAP_SHARED;
                  break;
          case 1:
                  c_mflag = MAP_PRIVATE;
                  break;
          default:
                  caml_invalid_argument ("maptype");
          }

        static_assert ((sizeof (struct mmap_interface) % sizeof (value)) ==
                       0);

        if (Int_val (len) < 0)
                caml_invalid_argument ("negative size");
        if (Int_val (offset) < 0)
                caml_invalid_argument ("negative offset");
        length = Long_val (len);

        int c_fd = Int_val (fd);
        size_t c_offset = Long_val (offset);

        caml_release_runtime_system ();
        addr = mmap (NULL, length, c_pflag, c_mflag, c_fd, c_offset);
        caml_acquire_runtime_system ();

        if (MAP_FAILED == addr)
                uerror ("mmap", Nothing);

        result = stub_mmap_alloc (addr, length);
        CAMLreturn (result);
}

CAMLprim value
stub_mmap_final (value intf)
{
        CAMLparam1 (intf);
        void *addr = Intf_val (intf)->addr;

        if (addr != MAP_FAILED)
          {
                  int len = Intf_val (intf)->len;
                  caml_release_runtime_system ();
                  munmap (addr, len);
                  caml_acquire_runtime_system ();
          }

        Intf_val (intf)->addr = MAP_FAILED;

        CAMLreturn (Val_unit);
}

CAMLprim value
stub_mmap_getpagesize (value unit)
{
        CAMLparam1 (unit);
        CAMLlocal1 (data);

        data = Val_int (getpagesize ());
        CAMLreturn (data);
}
