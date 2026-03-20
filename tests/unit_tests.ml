let initialize () =
  let store = Store.create () in
  let gnttab = Gnt.Gnttab.interface_open () in
  let advance_next_frequent_ops () = () in
  let domains_init eventchn =
    Domains.init eventchn gnttab advance_next_frequent_ops
  in
  let doms = domains_init @@ Event.init () in
  let cons = Connections.create () in
  (store, doms, cons)

let initialize_main_loop () =
  Domains.xenstored_port := "/tmp/port" ;
  let fd =
    Unix.openfile !Domains.xenstored_port [Unix.O_RDWR; Unix.O_CREAT] 0o600
  in
  let _ = Unix.write_substring fd "0" 0 1 in
  Unix.close fd ;
  Domains.xenstored_kva := "/tmp/kva" ;
  let fd =
    Unix.openfile !Domains.xenstored_kva [Unix.O_RDWR; Unix.O_CREAT] 0o600
  in
  Unix.close fd

let create_dom0_conn cons doms =
  (* NOTE: We can't use Domains.create0 since that opens several files
     unavailable in the test env *)
  let dom0 = Domains.create ~remote_port:0 doms 0 1337n in
  Connections.add_domain cons dom0 ;
  Hashtbl.find cons.domains 0

let create_domU_conn cons doms domid =
  let ndom = Domains.create ~remote_port:domid doms domid 1337n in
  Connections.add_domain cons ndom ;
  Hashtbl.find cons.domains domid

let none = Transaction.none

let op_testable =
  Alcotest.testable (Fmt.of_to_string Xenbus.Op.to_string) Stdlib.( = )

let terminate_with_null_char s = Printf.sprintf "%s\000" s

let check_result (reply : Xenbus.Packet.t)
    (expected : Xenbus.Op.operation * string list) =
  let expected_ty, expected_data = expected in
  let expected_data = String.concat "\000" expected_data in
  let expected_data =
    Xenbus.Op.(
      match reply.ty with
      | Read | Directory ->
          expected_data
      | _ ->
          terminate_with_null_char expected_data
    )
  in
  Alcotest.(check' op_testable)
    ~msg:"Verify response type is as expected" ~actual:reply.ty
    ~expected:expected_ty ;
  Alcotest.(check' string)
    ~msg:"Verify payload is as expected" ~actual:reply.data
    ~expected:expected_data

let rpc store cons doms con tid ty payload =
  let data = payload |> String.concat "\000" |> terminate_with_null_char in
  let req = Packet.{tid; rid= 0; ty; data} in
  Process.process_packet ~store ~cons ~doms ~con ~req ;
  Xenbus.Xb.unsafe_pop_output con.xb

let start_transaction store cons doms dom0 =
  (rpc store cons doms dom0 none Transaction_start [""]).data
  |> String.split_on_char '\000'
  |> List.hd
  |> int_of_string

(* Runs a sequence of calls, testing the reply to each one is as expected *)
let run store cons doms payloads =
  List.iter
    (fun (con, tid, (ty, payload), expected_result) ->
      check_result (rpc store cons doms con tid ty payload) expected_result
    )
    payloads

(* Write a path and check the parent nodecan be read *)
let test_implicit_create () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let domU = create_domU_conn cons doms 1 in

  run store cons doms
    [
      (* If a node doesn't exist, everyone gets ENOENT: *)
      (dom0, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ; (domU, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ; (* If dom0 makes a node, suddenly domU gets EACCES: *)
      (dom0, none, (Write, ["/a/b"; "hello"]), (Write, ["OK"]))
    ; (domU, none, (Read, ["/a/b"]), (Error, ["EACCES"]))
    ; (* dom0 can also see the implicit path created: *)
      (dom0, none, (Read, ["/a"]), (Read, [""]))
    ; (* domU gets EACCES: *)
      (domU, none, (Read, ["/a"]), (Error, ["EACCES"]))
    ]

(* We do not preserve ordering of nodes in a directory *)
let _test_directory_order () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  (* Create nodes in a particular order and check 'directory' preserves the ordering *)
  run store cons doms
    [
      (dom0, none, (Write, ["/a/2/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/a/1"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/a/3"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Directory, ["/a"]), (Directory, [""]))
    ]

let example_acl = ["r5"; "w2"; "b3"]

(* Check that getperms(setperms(x)) = x *)
let test_setperms_getperms () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (dom0, none, (Getperms, ["/foo"]), (Getperms, ["r5"; "w2"; "b3"]))
    ]

(* Check that no domain other than dom0 can change owners of the node
   even if another domain has read/write access *)
let test_setperms_owner () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom2 = create_domU_conn cons doms 2 in
  let dom5 = create_domU_conn cons doms 5 in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (* owned by dom5, so dom2 can't setperms *)
      (dom2, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Error, ["EACCES"]))
    ; (* dom5 can't change owners since it's not dom0 - XSA-115 *)
      (dom5, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Error, ["EACCES"]))
    ; (* only dom0 can change owners - XSA-115 *)
      (dom0, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Setperms, ["OK"]))
    ]

(* Check that mkdir creates usable nodes *)
let test_mkdir () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Read, ["/a/b"]), (Error, ["ENOENT"]))
    ; (dom0, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ] ;
  let tid = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid, (Mkdir, ["/bench/local/domain/0"]), (Mkdir, ["OK"]))
    ; ( dom0
      , tid
      , (Setperms, ["/bench/local/domain/0"; "r5"; "w2"; "b3"])
      , (Setperms, ["OK"])
      )
    ; (dom0, tid, (Read, ["/bench/local/domain/0"]), (Read, [""]))
    ; (dom0, tid, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ]

(* Check that I can read an empty value *)
let test_empty () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Write, ["/a"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Read, ["/a"]), (Read, ["\000"]))
    ]

(* rm of a missing node from an existing parent should succeed
   rm of a missing node from a missing parent should ENOENT *)
let test_rm () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Rm, ["/a"]), (Rm, ["OK"]))
    ; (dom0, none, (Rm, ["/a/b"]), (Error, ["ENOENT"]))
    ; (dom0, none, (Write, ["/a"; "hello"]), (Write, ["OK"]))
    ; (dom0, none, (Rm, ["/a/b"]), (Rm, ["OK"]))
    ]

(* Check that dom0 can grant dom1 access to dom2's nodes,
   without which it wouldn't have access. *)
let test_set_target () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom7 = create_domU_conn cons doms 7 in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; "bar"]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (dom7, none, (Write, ["/foo"; "bar"]), (Error, ["EACCES"]))
    ; (dom0, none, (Set_target, ["7"; "5"]), (Set_target, ["OK"]))
    ; (* Any other domain can't use Set_target *)
      (dom7, none, (Set_target, ["7"; "5"]), (Error, ["EACCES"]))
    ; (dom7, none, (Write, ["/foo"; "bar"]), (Write, ["OK"]))
    ]

(* Check that other connections cannot see the nodes created
   within an uncommitted transaction *)
let test_transactions_are_isolated () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let tid = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid, (Write, ["/foo"; "bar"]), (Write, ["OK"]))
    ; (dom0, none, (Read, ["/foo"]), (Error, ["ENOENT"]))
    ; (dom0, tid, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; (dom0, none, (Read, ["/foo"]), (Read, ["bar\000"]))
    ]

(* Check that two parallel, unrelated transactions can be
   coalesced properly *)
let test_independent_transactions_coalesce () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Mkdir, ["/a/b"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Mkdir, ["/1/2"]), (Mkdir, ["OK"]))
    ] ;
  let tid_1 = start_transaction store cons doms dom0 in
  let tid_2 = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid_1, (Write, ["/a/b"; "foo"]), (Write, ["OK"]))
    ; (dom0, tid_2, (Write, ["/1/2"; "foo"]), (Write, ["OK"]))
    ; (dom0, tid_1, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; (dom0, tid_2, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; (dom0, none, (Read, ["/a/b"]), (Read, ["foo\000"]))
    ; (dom0, none, (Read, ["/1/2"]), (Read, ["foo\000"]))
    ]

(* Check that two parallel, device-creating transactions can coalesce *)
let test_device_create_coalesce () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Mkdir, ["/local/domain/0/backend/vbd"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Mkdir, ["/local/domain/1/device/vbd"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Mkdir, ["/local/domain/2/device/vbd"]), (Mkdir, ["OK"]))
    ] ;
  let tid_1 = start_transaction store cons doms dom0 in
  let tid_2 = start_transaction store cons doms dom0 in
  run store cons doms
    [
      ( dom0
      , tid_1
      , (Write, ["/local/domain/0/backend/vbd/1/51712"; "hello"])
      , (Write, ["OK"])
      )
    ; ( dom0
      , tid_1
      , (Write, ["/local/domain/1/device/vbd/51712"; "there"])
      , (Write, ["OK"])
      )
    ; ( dom0
      , tid_2
      , (Write, ["/local/domain/0/backend/vbd/2/51712"; "hello"])
      , (Write, ["OK"])
      )
    ; ( dom0
      , tid_2
      , (Write, ["/local/domain/2/device/vbd/51712"; "there"])
      , (Write, ["OK"])
      )
    ; (dom0, tid_1, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; (dom0, tid_2, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; ( dom0
      , none
      , (Read, ["/local/domain/0/backend/vbd/1/51712"])
      , (Read, ["hello\000"])
      )
    ; ( dom0
      , none
      , (Read, ["/local/domain/1/device/vbd/51712"])
      , (Read, ["there\000"])
      )
    ; ( dom0
      , none
      , (Read, ["/local/domain/0/backend/vbd/2/51712"])
      , (Read, ["hello\000"])
      )
    ; ( dom0
      , none
      , (Read, ["/local/domain/2/device/vbd/51712"])
      , (Read, ["there\000"])
      )
    ]

(* Check that transactions that really can't interleave are aborted *)
let test_transactions_really_do_conflict () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms [(dom0, none, (Mkdir, ["/a"]), (Mkdir, ["OK"]))] ;
  let tid = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid, (Directory, ["/a"]), (Directory, [""]))
    ; (dom0, none, (Write, ["/a/b"; "hello"]), (Write, ["OK"]))
    ; (dom0, tid, (Write, ["/a/b"; "there"]), (Write, ["OK"]))
    ; (dom0, tid, (Transaction_end, ["T"]), (Error, ["EAGAIN"]))
    ; (dom0, none, (Read, ["/a/b"]), (Read, ["hello\000"]))
    ]

let assert_watches c expected =
  Alcotest.(check' (list @@ triple string string (option int)))
    ~msg:"Check connection's watches are as expected"
    ~actual:(Connection.list_watches c)
    ~expected

let check_for_watchevent (con : Connection.t) path token =
  let actual = Xenbus.Xb.unsafe_pop_output con.xb in
  check_result actual (Watchevent, [path; token])

let check_no_watchevents (con : Connection.t) =
  Alcotest.(check' int)
    ~msg:"Verifying queue is empty, without any watchevents"
    ~actual:(Xenbus.Xb.output_len con.xb)
    ~expected:0

(* Check that writes generate watches and reads do not *)
let test_simple_watches () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in

  (* No watch events are generated without registering *)
  run store cons doms
    [
      (dom0, none, (Mkdir, ["/a"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Setperms, ["/a"; "b0"]), (Setperms, ["OK"]))
    ] ;
  assert_watches dom0 [] ;

  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  run store cons doms [(dom0, none, (Watch, ["/a"; "token"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("/a", "token", None)] ;
  check_for_watchevent dom0 "/a" "token" ;

  (* dom0 can see its own write via watches *)
  run store cons doms [(dom0, none, (Write, ["/a"; "foo"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a" "token" ;

  (* dom0 can see dom1's writes via watches *)
  run store cons doms [(dom1, none, (Write, ["/a"; "foo"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a" "token" ;

  (* reads don't generate watches *)
  run store cons doms
    [
      (dom0, none, (Read, ["/a"]), (Read, ["foo\000"]))
    ; (dom0, none, (Read, ["/a/1"]), (Error, ["ENOENT"]))
    ; (dom1, none, (Read, ["/a"]), (Read, ["foo\000"]))
    ; (dom1, none, (Read, ["/a/1"]), (Error, ["ENOENT"]))
    ]

(* Check watches on relative paths *)
let test_relative_watches () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  (* No watch events are generated without registering *)
  run store cons doms
    [
      (dom0, none, (Write, ["/local/domain/0/name"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/local/domain/0/device"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Watch, ["device"; "token"]), (Watch, ["OK"]))
    ] ;
  assert_watches dom0 [("device", "token", None)] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "device" "token" ;

  (* dom0 should see the absolute write on a relative path watch *)
  run store cons doms
    [
      ( dom0
      , none
      , (Write, ["/local/domain/0/device/vbd"; "hello"])
      , (Write, ["OK"])
      )
    ] ;
  check_for_watchevent dom0 "device/vbd" "token" ;
  assert_watches dom0 [("device", "token", None)]

(* Check that a connection only receives a watch if it
   can read the node that was modified. *)
let test_watches_read_perm () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in

  run store cons doms [(dom1, none, (Watch, ["/a"; "token"]), (Watch, ["OK"]))] ;
  assert_watches dom1 [("/a", "token", None)] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom1 "/a" "token" ;

  run store cons doms
    [
      (dom0, none, (Write, ["/a"; "hello"]), (Write, ["OK"]))
    ; (dom1, none, (Read, ["/a"]), (Error, ["EACCES"]))
    ] ;
  assert_watches dom1 [("/a", "token", None)] ;
  check_no_watchevents dom1

(* Check that watches only appear on transaction commit
   and not at all in the case of abort *)
let test_transaction_watches () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms [(dom0, none, (Watch, ["/a"; "token"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("/a", "token", None)] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "/a" "token" ;

  (* Writes in a transaction don't generate watches immediately *)
  let tid = start_transaction store cons doms dom0 in
  run store cons doms [(dom0, tid, (Write, ["/a"; "hello"]), (Write, ["OK"]))] ;
  check_no_watchevents dom0 ;

  (* If the transaction is aborted then no watches are generated *)
  run store cons doms
    [(dom0, tid, (Transaction_end, ["F"]), (Transaction_end, ["OK"]))] ;
  check_no_watchevents dom0 ;

  (* If the transaction successfully commits then the watches appear *)
  let tid = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid, (Write, ["/a"; "hello"]), (Write, ["OK"]))
    ; (* Watchevent is pushed to the queue first, then Transaction_end *)
      (dom0, tid, (Transaction_end, ["T"]), (Watchevent, ["/a"; "token"]))
    ] ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Transaction_end, ["OK"])

let register_spec_watches store cons doms dom0 =
  (* Register both special watches *)
  run store cons doms
    [(dom0, none, (Watch, ["@introduceDomain"; "token"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("@introduceDomain", "token", None)] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "@introduceDomain" "token" ;

  run store cons doms
    [(dom0, none, (Watch, ["@releaseDomain"; "token"]), (Watch, ["OK"]))] ;
  assert_watches dom0
    [("@releaseDomain", "token", None); ("@introduceDomain", "token", None)] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "@releaseDomain" "token"

(* Check that @introduceDomain and @releaseDomain watches appear on respective calls *)
let test_introduce_release_watches () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in

  register_spec_watches store cons doms dom0 ;

  (* Watchevent is pushed to the queue first, then Introduce *)
  run store cons doms
    [
      ( dom0
      , none
      , (Introduce, ["5"; "5"; "5"])
      , (Watchevent, ["@introduceDomain"; "token"])
      )
    ] ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Introduce, ["OK"]) ;

  (* Watchevent is pushed to the queue first, then Release *)
  run store cons doms
    [(dom0, none, (Release, ["5"]), (Watchevent, ["@releaseDomain"; "token"]))] ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Release, ["OK"])

(* Check that @introduceDomain and @releaseDomain watches appear on domains dying *)
let test_introduce_release_watches_on_domain_death () =
  (* Only the Xenstored.main loop checks for domains dying, so we need to
     do special handling here unlike any other unit test *)
  initialize_main_loop () ;
  let one_loop_iteration, store, cons, doms = Xenstored.main () in
  let dom0 = Hashtbl.find cons.domains 0 in

  register_spec_watches store cons doms dom0 ;

  (* Domains in [1000; 2000] are considered shutdown on the first query *)
  let _ = create_domU_conn cons doms 1001 in
  (* Check that a @releaseDomain watch event is sent when a domain shuts down *)
  one_loop_iteration () ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain"; "token"]) ;
  check_no_watchevents dom0 ;

  (* Domains > 2000 are considered dead on the first query *)
  let _ = create_domU_conn cons doms 2001 in
  let _ = create_domU_conn cons doms 2002 in

  (* Check that only a single @releaseDomain watch event is sent
     when multiple domains die *)
  one_loop_iteration () ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain"; "token"]) ;
  check_no_watchevents dom0 ;

  (* Register a releaseDomain with depth=1 *)
  run store cons doms
    [
      ( dom0
      , none
      , (Watch, ["@releaseDomain"; "tokendepth"; "1"])
      , (Watch, ["OK"])
      )
    ] ;
  assert_watches dom0
    [
      ("@releaseDomain", "tokendepth", Some 1)
    ; ("@releaseDomain", "token", None)
    ; ("@introduceDomain", "token", None)
    ] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "@releaseDomain" "tokendepth" ;

  (* Verify that detailed watch events are sent when multiple domains die *)
  let _ = create_domU_conn cons doms 2003 in
  let _ = create_domU_conn cons doms 2004 in
  one_loop_iteration () ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain/2004"; "tokendepth"]) ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain"; "token"]) ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain/2003"; "tokendepth"]) ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Watchevent, ["@releaseDomain/1001"; "tokendepth"]) ;
  check_no_watchevents dom0

(* Check that rm generates recursive watches *)
let test_recursive_rm_watch () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in

  run store cons doms
    [
      (dom0, none, (Mkdir, ["/a/b/c/d"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Write, ["/a/b/y/z"; "hello"]), (Write, ["OK"]))
    ; (dom0, none, (Watch, ["/a/b/c"; "token"]), (Watch, ["OK"]))
    ] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "/a/b/c" "token" ;
  assert_watches dom0 [("/a/b/c", "token", None)] ;

  run store cons doms
    [(dom0, none, (Watch, ["/a/b/y/z"; "token"]), (Watch, ["OK"]))] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "/a/b/y/z" "token" ;
  assert_watches dom0 [("/a/b/c", "token", None); ("/a/b/y/z", "token", None)] ;

  (* Check that removing a parent node triggers watches recursively
     down into the children *)
  run store cons doms [(dom0, none, (Rm, ["/a"]), (Rm, ["OK"]))] ;
  check_for_watchevent dom0 "/a/b/c" "token" ;
  check_for_watchevent dom0 "/a/b/y/z" "token"

(* Check that watches implement a depth parameter properly.
   From documentation upstream:
    The event's path matches the watch's <wpath> if it is an child
    of <wpath>. This match can be limited by specifying <depth> (a
    decimal value of 0 or larger): it denotes the directory levels
    below <wpath> to consider for a match ("0" would not match for
    a child of <wpath>, "1" would match only for a direct child,
    etc.). *)
let test_watches_depth () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in

  (* No watch events are generated without registering *)
  run store cons doms
    [
      (dom0, none, (Mkdir, ["/a"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Setperms, ["/a"; "b0"]), (Setperms, ["OK"]))
    ] ;
  assert_watches dom0 [] ;

  (* Check that a watch with a depth parameter is added without errors *)
  run store cons doms
    [(dom0, none, (Watch, ["/a"; "token"; "1"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("/a", "token", Some 1)] ;
  check_for_watchevent dom0 "/a" "token" ;

  (* Adding another watch on the same connection with the same path and token,
     but with a different depth fails *)
  run store cons doms
    [(dom0, none, (Watch, ["/a"; "token"; "3"]), (Error, ["EEXIST"]))] ;
  assert_watches dom0 [("/a", "token", Some 1)] ;

  (* Adding a watch with a negative depth fails *)
  run store cons doms
    [(dom0, none, (Watch, ["/b"; "token-1"; "-1"]), (Error, ["EINVAL"]))] ;
  assert_watches dom0 [("/a", "token", Some 1)] ;

  (* Adding another watch with a different token at a different depth succeeds *)
  run store cons doms
    [(dom0, none, (Watch, ["/a"; "token2"; "2"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("/a", "token2", Some 2); ("/a", "token", Some 1)] ;
  check_for_watchevent dom0 "/a" "token2" ;

  (* Add a zero-depth watch *)
  run store cons doms
    [(dom0, none, (Watch, ["/a"; "token0"; "0"]), (Watch, ["OK"]))] ;
  assert_watches dom0
    [
      ("/a", "token0", Some 0); ("/a", "token2", Some 2); ("/a", "token", Some 1)
    ] ;
  check_for_watchevent dom0 "/a" "token0" ;

  (* Check that the right watches are triggered depending on the depth of writes *)
  run store cons doms [(dom0, none, (Write, ["/a"; "foo"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a" "token0" ;
  check_for_watchevent dom0 "/a" "token2" ;
  check_for_watchevent dom0 "/a" "token" ;
  run store cons doms [(dom1, none, (Write, ["/a"; "baz"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a" "token0" ;
  check_for_watchevent dom0 "/a" "token2" ;
  check_for_watchevent dom0 "/a" "token" ;
  run store cons doms [(dom0, none, (Write, ["/a/b"; "foo"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a/b" "token2" ;
  check_for_watchevent dom0 "/a/b" "token" ;
  run store cons doms [(dom1, none, (Write, ["/a/c"; "baz"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a/c" "token2" ;
  check_for_watchevent dom0 "/a/c" "token" ;

  (* Check that only the deeper watch is triggered with a write to a grandchild node *)
  run store cons doms
    [(dom0, none, (Write, ["/a/b/c"; "foo"]), (Write, ["OK"]))] ;
  check_for_watchevent dom0 "/a/b/c" "token2" ;

  (* reads don't generate watches *)
  run store cons doms
    [
      (dom0, none, (Read, ["/a"]), (Read, ["baz\000"]))
    ; (dom0, none, (Read, ["/a/1"]), (Error, ["ENOENT"]))
    ; (dom1, none, (Read, ["/a"]), (Read, ["baz\000"]))
    ; (dom1, none, (Read, ["/a/1"]), (Error, ["ENOENT"]))
    ]

(* Check that @introduceDomain and @releaseDomain watches appear on
   respective calls and depth is well-handled.
   From documentation upstream:
    The semantics for a specification of <depth> differ for generating
    <wspecial> events: specifying "1" will report the related domid by using
    @<wspecial>/<domid> for the reported path. Other <depth>
    values are not supported. *)
let test_special_watches_depth () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in

  (* Adding a special watch with a negative depth fails *)
  run store cons doms
    [
      ( dom0
      , none
      , (Watch, ["@introduceDomain"; "token"; "-1"])
      , (Error, ["EINVAL"])
      )
    ] ;
  assert_watches dom0 [] ;

  (* Adding a special watch with a depth other than 1 fails *)
  run store cons doms
    [
      ( dom0
      , none
      , (Watch, ["@releaseDomain"; "token"; "0"])
      , (Error, ["EINVAL"])
      )
    ] ;
  assert_watches dom0 [] ;

  (* Adding a @releaseDomain/domid with any depth fails *)
  run store cons doms
    [
      ( dom0
      , none
      , (Watch, ["@releaseDomain/5"; "token"; "1"])
      , (Error, ["EINVAL"])
      )
    ] ;
  assert_watches dom0 [] ;

  (* Adding a special watch with depth=1 works *)
  run store cons doms
    [(dom0, none, (Watch, ["@introduceDomain"; "token"; "1"]), (Watch, ["OK"]))] ;
  assert_watches dom0 [("@introduceDomain", "token", Some 1)] ;

  check_for_watchevent dom0 "@introduceDomain" "token" ;

  run store cons doms
    [(dom0, none, (Watch, ["@releaseDomain"; "token"; "1"]), (Watch, ["OK"]))] ;
  assert_watches dom0
    [("@releaseDomain", "token", Some 1); ("@introduceDomain", "token", Some 1)] ;
  check_for_watchevent dom0 "@releaseDomain" "token" ;

  run store cons doms
    [(dom0, none, (Watch, ["@releaseDomain/5"; "token2"]), (Watch, ["OK"]))] ;
  assert_watches dom0
    [
      ("@releaseDomain", "token", Some 1)
    ; ("@introduceDomain", "token", Some 1)
    ; ("@releaseDomain/5", "token2", None)
    ] ;
  check_for_watchevent dom0 "@releaseDomain/5" "token2" ;

  (* Watchevents generated contain the new domid *)
  run store cons doms
    [
      ( dom0
      , none
      , (Introduce, ["5"; "5"; "5"])
      , (Watchevent, ["@introduceDomain/5"; "token"])
      )
    ] ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Introduce, ["OK"]) ;

  run store cons doms
    [
      (dom0, none, (Release, ["5"]), (Watchevent, ["@releaseDomain/5"; "token"]))
    ] ;
  check_for_watchevent dom0 "@releaseDomain/5" "token2" ;
  let actual = Xenbus.Xb.unsafe_pop_output dom0.xb in
  check_result actual (Release, ["OK"])

(* Check that a write failure doesn't generate a watch *)
let test_no_watch_on_error () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in
  run store cons doms
    [
      (dom0, none, (Mkdir, ["/a"]), (Mkdir, ["OK"]))
    ; (dom0, none, (Watch, ["/a"; "token"]), (Watch, ["OK"]))
    ] ;
  (* One Watchevent is fired immediately after adding the watch unconditionally *)
  check_for_watchevent dom0 "/a" "token" ;

  run store cons doms
    [(dom1, none, (Write, ["/a/b/y/z"; "hello"]), (Error, ["EACCES"]))] ;
  check_no_watchevents dom0 ;

  run store cons doms
    [
      (dom0, none, (Setperms, ["/a"; "r1"]), (Setperms, ["OK"]))
    ; (dom1, none, (Write, ["/a/b/y/z"; "hello"]), (Write, ["OK"]))
    ] ;
  check_for_watchevent dom0 "/a" "token"

(* TODO: Cxenstored has a more complex quota system, with controllable limits
   for watches, number of permissions, etc. Oxenstored does not, so some of the quota
   tests from the Mirage ocaml-xenstore repo do not apply to us. Yet *)

let check_quota_ent_per_domain store ~domid expected =
  let get_current_entries_quota store domid =
    Quota.find_or_zero store.Store.quota.cur domid
  in
  Alcotest.(check' int)
    ~msg:"Verify quota usage is as expected"
    ~actual:(get_current_entries_quota store domid)
    ~expected

(* Check that node creation and destruction changes a quota *)
let test_quota () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in

  run store cons doms [(dom0, none, (Write, ["/a"; "hello"]), (Write, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:0 1 ;

  (* Implicit creation of 2 elements *)
  run store cons doms
    [(dom0, none, (Write, ["/a/b/c"; "hello"]), (Write, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:0 3 ;

  (* Remove one element *)
  run store cons doms [(dom0, none, (Rm, ["/a/b/c"]), (Rm, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:0 2 ;

  (* Recursive remove of 2 elements *)
  run store cons doms [(dom0, none, (Rm, ["/a"]), (Rm, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:0 0 ;

  (* Remove an already removed element *)
  run store cons doms [(dom0, none, (Rm, ["/a"]), (Rm, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:0 0

(* Check that node creation and destruction in a transaction changes a quota *)
let test_quota_transaction () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in
  let dom2 = create_domU_conn cons doms 2 in

  run store cons doms
    [
      (dom0, none, (Write, ["/local/domain/1"; ""]), (Write, ["OK"]))
    ; ( dom0
      , none
      , (Setperms, ["/local/domain/1"; "r1"; "w2"; "b3"])
      , (Setperms, ["OK"])
      )
    ; (dom0, none, (Write, ["/local/domain/2"; ""]), (Write, ["OK"]))
    ; ( dom0
      , none
      , (Setperms, ["/local/domain/2"; "r2"; "w2"; "b3"])
      , (Setperms, ["OK"])
      )
    ; (dom1, none, (Write, ["/local/domain/1/data/test"; ""]), (Write, ["OK"]))
    ] ;
  check_quota_ent_per_domain store ~domid:1 3 ;

  run store cons doms
    [
      ( dom1
      , none
      , (Write, ["/local/domain/1/data/test/node0"; "node0"])
      , (Write, ["OK"])
      )
    ] ;
  check_quota_ent_per_domain store ~domid:1 4 ;

  run store cons doms
    [(dom2, none, (Write, ["/local/domain/2/data/test"; ""]), (Write, ["OK"]))] ;
  check_quota_ent_per_domain store ~domid:2 3 ;

  let tid_1 = start_transaction store cons doms dom1 in
  let tid_2 = start_transaction store cons doms dom2 in
  run store cons doms
    [
      (dom1, tid_1, (Rm, ["/local/domain/1/data/test"]), (Rm, ["OK"]))
    ; ( dom2
      , tid_2
      , (Write, ["/local/domain/2/data/test/node0"; "node0"])
      , (Write, ["OK"])
      )
    ] ;

  (* Transactions have not yet been committed *)
  check_quota_ent_per_domain store ~domid:1 4 ;
  check_quota_ent_per_domain store ~domid:2 3 ;

  (* Transactions committed, nodes removed and added *)
  run store cons doms
    [
      (dom1, tid_1, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ; (dom2, tid_2, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ] ;
  check_quota_ent_per_domain store ~domid:1 2 ;
  check_quota_ent_per_domain store ~domid:2 4

(* Check that string length quota is checked correctly *)
let test_quota_maxsize () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in

  (* Length check includes the null byte *)
  store.quota <- {store.quota with maxsize= 6} ;
  run store cons doms
    [
      (dom0, none, (Write, ["/a"; "hello"]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/a"; "hello2"]), (Error, ["E2BIG"]))
    ] ;

  store.quota <- {store.quota with maxsize= 7} ;

  run store cons doms [(dom0, none, (Write, ["/a"; "hello2"]), (Write, ["OK"]))]

(* Check that number of nodes per domain quota is checked correctly *)
let test_quota_maxent () =
  let store, doms, cons = initialize () in
  (* Quota for dom0 is ignored, so test for DomU *)
  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in

  store.quota <- {store.quota with maxent= 2} ;
  run store cons doms
    [
      (dom0, none, (Write, ["/local/domain/1"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/local/domain/1"; "r1"]), (Setperms, ["OK"]))
    ] ;

  run store cons doms
    [
      (dom1, none, (Write, ["first"; "post"]), (Write, ["OK"]))
    ; (dom1, none, (Write, ["a"; "hello"]), (Error, ["EQUOTA"]))
    ] ;

  store.quota <- {store.quota with maxent= 3} ;
  run store cons doms
    [
      (dom1, none, (Write, ["a"; "hello"]), (Write, ["OK"]))
    ; (dom1, none, (Write, ["a"; "there"]), (Write, ["OK"]))
    ; (dom1, none, (Write, ["b"; "hello"]), (Error, ["EQUOTA"]))
    ]

let feature_testable =
  Alcotest.testable
    (Fmt.of_to_string Xenbus.Xs_ring.Server_feature.to_string)
    Stdlib.( = )

let check_features (con : Connection.t) expected =
  let open Xenbus.Xs_ring in
  let d = Option.get (Connection.get_domain con) in
  let x = get_server_features (Xenmmap.to_interface (Domain.get_interface d)) in
  Alcotest.(check' (seq @@ feature_testable))
    ~msg:"Verify advertised features on xenstore ring"
    ~actual:(Server_features.to_seq x) ~expected:(List.to_seq expected)

let test_feature_advertisement () =
  let _store, doms, cons = initialize () in
  (* Turn off under_testing flag here to trigger the setting of feature bitmap *)
  Testing_status.under_testing := false ;

  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in
  check_features dom0 Xenbus.Xs_ring.Server_feature.[Reconnection; Watch_depth] ;
  check_features dom1 Xenbus.Xs_ring.Server_feature.[Reconnection; Watch_depth] ;
  Testing_status.under_testing := true

let test_get_feature () =
  let store, doms, cons = initialize () in
  (* Turn off under_testing flag here to trigger the setting of feature bitmap *)
  Testing_status.under_testing := false ;

  let dom0 = create_dom0_conn cons doms in
  let dom1 = create_domU_conn cons doms 1 in
  run store cons doms
    [
      (dom0, none, (Get_feature, ["0"]), (Error, ["ENOSYS"]))
    ; (dom1, none, (Get_feature, ["1"]), (Error, ["ENOSYS"]))
    ; (dom1, none, (Set_feature, ["0"; "1"]), (Error, ["ENOSYS"]))
    ] ;
  check_features dom0 Xenbus.Xs_ring.Server_feature.[Reconnection; Watch_depth] ;
  check_features dom1 Xenbus.Xs_ring.Server_feature.[Reconnection; Watch_depth] ;
  Testing_status.under_testing := true

let () =
  Alcotest.run "Test oxenstored"
    [
      ( "Basic tests"
      , [
          ("test_implicit_create", `Quick, test_implicit_create)
        ; ("getperms(setperms)", `Quick, test_setperms_getperms)
        ; ("test_setperms_owner", `Quick, test_setperms_owner)
        ; ("test_mkdir", `Quick, test_mkdir)
        ; ("test_empty", `Quick, test_empty)
        ; ("test_rm", `Quick, test_rm)
        ; ("test_set_target", `Quick, test_set_target)
        ]
      )
    ; ( "Transaction tests"
      , [
          ("transactions_are_isolated", `Quick, test_transactions_are_isolated)
        ; ( "independent_transactions_coalesce"
          , `Quick
          , test_independent_transactions_coalesce
          )
        ; ("device_create_coalesce", `Quick, test_device_create_coalesce)
        ; ( "test_transactions_really_do_conflict"
          , `Quick
          , test_transactions_really_do_conflict
          )
        ]
      )
    ; ( "Watches tests"
      , [
          ("test_simple_watches", `Quick, test_simple_watches)
        ; ("test_relative_watches", `Quick, test_relative_watches)
        ; ("test_watches_read_perm", `Quick, test_watches_read_perm)
        ; ("test_transaction_watches", `Quick, test_transaction_watches)
        ; ( "test_introduce_release_watches"
          , `Quick
          , test_introduce_release_watches
          )
        ; ( "test_introduce_release_watches_on_domain_death"
          , `Quick
          , test_introduce_release_watches_on_domain_death
          )
        ; ("test_recursive_rm_watch", `Quick, test_recursive_rm_watch)
        ; ("test_no_watch_on_error", `Quick, test_no_watch_on_error)
        ; ("test_watches_depth", `Quick, test_watches_depth)
        ; ("test_special_watches_depth", `Quick, test_special_watches_depth)
        ]
      )
    ; ( "Quota tests"
      , [
          ("test_quota", `Quick, test_quota)
        ; ("test_quota_transaction", `Quick, test_quota_transaction)
        ; ("test_quota_maxsize", `Quick, test_quota_maxsize)
        ; ("test_quota_maxent", `Quick, test_quota_maxent)
        ]
      )
    ; ( "Features tests"
      , [
          ("test_feature_advertisement", `Quick, test_feature_advertisement)
        ; ("test_get_feature", `Quick, test_get_feature)
        ]
      )
    ]
