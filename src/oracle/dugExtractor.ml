(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Dug

type 'a wrap = {
    data : 'a;
    id : Id.t;
  }

let mutate t op f wrap = 
  let data = f wrap.data in
  let id = fresh_id t in
  let kop = Mutator (op id) in
  let e = {target = id;op = kop; pos = 0} in
    Hashtbl.add t.nodes id kop;
    Hashtbl.add t.edges wrap.id e;
    {data=data; id=id}

let mutate2 t op f w1 w2 = 
  let id = fresh_id t in
  let kop = Mutator (op id) in
  let e1 = {target = id; op = kop; pos = 0} in
  let e2 = {e1 with pos = 1} in
    Hashtbl.add t.nodes id kop;
    Hashtbl.add t.edges w1.id e1;
    Hashtbl.add t.edges w2.id e2;
    {data = f w1.data w2.data; id=id}
      
let observe t op f w = 
  let kop = Observer op in
  let id' = fresh_id t in
  let e = {target = id'; op = kop; pos = 0} in
    Hashtbl.add t.nodes id' kop;
    Hashtbl.add t.edges w.id e;
    f w.data
      
let observe2 t op f w1 w2 =
  let kop = Observer op in
  let id' = fresh_id t in
  let e1 = {target = id'; op = kop; pos = 0} in
  let e2 = {e1 with pos = 1} in
    Hashtbl.add t.nodes id' kop;
    Hashtbl.add t.edges w1.id e1;
    Hashtbl.add t.edges w2.id e2;
    f w1.data w2.data

let generate t op data = 
  let id = fresh_id t in
    Hashtbl.add t.nodes id (Generator (op id));
    {data = data; id=id}




