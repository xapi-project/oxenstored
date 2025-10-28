/*
 * Copyright (C) 2008-2014 Citrix Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <sys/ioctl.h>
#include <string.h>

#include <xenevtchn.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>

#define _H(__h) ((xenevtchn_handle *)(__h))
#define XENEVTCHN_NO_CLOEXEC (1 << 0)

xenevtchn_handle *global_xce = NULL;

CAMLprim value
stub_evtchn_init (value cloexec)
{
        CAMLparam1 (cloexec);
        unsigned int flags = 0;
        if (!Bool_val (cloexec))
                flags |= XENEVTCHN_NO_CLOEXEC;

        if (global_xce == NULL)
          {
                  global_xce = xenevtchn_open (NULL, flags);
          }

        if (global_xce == NULL)
                caml_failwith (strerror (errno));

        CAMLreturn ((value) global_xce);
}

CAMLprim value
stub_evtchn_fd (value xce)
{
        CAMLparam1 (xce);
        int fd;

        fd = xenevtchn_fd (_H (xce));
        if (fd == -1)
          {
                  perror ("xc_evtchn_fd");
                  caml_failwith (strerror (errno));
          }
        CAMLreturn (Val_int (fd));
}

CAMLprim value
stub_evtchn_notify (value xce, value port)
{
        CAMLparam2 (xce, port);
        if (xenevtchn_notify (_H (xce), Int_val (port)) == -1)
          {
                  perror ("xc_evtchn_notify");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_unit);
}

CAMLprim value
stub_evtchn_bind_interdomain (value xce, value domid, value remote_port)
{
        CAMLparam3 (xce, domid, remote_port);
        xenevtchn_port_or_error_t rc;

        rc = xenevtchn_bind_interdomain (_H (xce), Int_val (domid),
                                         Int_val (remote_port));
        if (rc == -1)
          {
                  perror ("xc_evtchn_bind_interdomain");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_int (rc));
}

CAMLprim value
stub_evtchn_alloc_unbound (value xce, value remote_domid)
{
        CAMLparam2 (xce, remote_domid);
        xenevtchn_port_or_error_t rc;

        rc = xenevtchn_bind_unbound_port (_H (xce), Int_val (remote_domid));
        if (rc == -1)
          {
                  perror ("xc_evtchn_bind_unbound_port");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_int (rc));

}

CAMLprim value
stub_evtchn_virq_dom_exc (value unit)
{
        CAMLparam1 (unit);
        CAMLreturn (Val_int (VIRQ_DOM_EXC));
}

CAMLprim value
stub_evtchn_bind_virq (value xce, value virq)
{
        CAMLparam2 (xce, virq);
        xenevtchn_port_or_error_t rc;

        rc = xenevtchn_bind_virq (_H (xce), Int_val (virq));
        if (rc == -1)
          {
                  perror ("xc_evtchn_bind_virq");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_int (rc));
}

CAMLprim value
stub_evtchn_unbind (value xce, value port)
{
        CAMLparam2 (xce, port);
        if (xenevtchn_unbind (_H (xce), Int_val (port)) == -1)
          {
                  perror ("xc_evtchn_unbind");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_unit);
}

CAMLprim value
stub_evtchn_pending (value xce)
{
        CAMLparam1 (xce);
        CAMLlocal1 (generation);
        xenevtchn_port_or_error_t port;

        generation = caml_alloc_tuple (2);

        port = xenevtchn_pending (_H (xce));
        if (port == -1)
          {
                  perror ("xc_evtchn_pending");
                  caml_failwith (strerror (errno));
          }

        Store_field (generation, 0, Val_int (0));
        Store_field (generation, 1, Val_int (port));

        CAMLreturn (generation);
}

CAMLprim value
stub_evtchn_unmask (value xce, value port)
{
        CAMLparam2 (xce, port);
        if (xenevtchn_unmask (_H (xce), Int_val (port)) == -1)
          {
                  perror ("xc_evtchn_unmask");
                  caml_failwith (strerror (errno));
          }

        CAMLreturn (Val_unit);
}
