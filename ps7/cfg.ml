open Cfg_ast
exception Implement_Me
exception FatalError


type igraph_node = RegNode of Mips.reg | VarNode of var

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> Mips.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   
(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect MIPS calling
   conventions *)

let call_gen_list = ["$4";"$5";"$6";"$7"]
let call_kill_list = ["$1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";
                      "$11";"$12";"$13";"$14";"$15";"$24";"$25";"$31"]

(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "%s : {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}" rows
  

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)

module LabelSet = Set.Make(struct
  type t = label
  let compare = String.compare
end)

module VarSet = Set.Make(struct
  type t = operand
  let compare t1 t2 = 
    let t1' = match t1 with
      | Reg(t1'') -> Mips.reg2string t1''
      | Var(v) -> v
      | Lab(v) -> v
      | _ -> print_endline "0";print_endline (op2string t1);raise Implement_Me
    in
    let t2' = match t2 with
      | Reg(t2'') -> Mips.reg2string t2''
      | Var(v) -> v
      | Lab(v) -> v
      | _ -> print_endline "1";raise Implement_Me
    in
    String.compare t1' t2'
end)

(* label -> Varset *)
module LiveMap = Map.Make(struct
  type t = label
  let compare = String.compare
end)

(* label -> Varset *)
(* module GKMap = Map.Make(struct
  type t = label
  let compare = String.compare
end) *)
module GKMap = LiveMap
(*for initializing liveinmap*)

(* label -> LabelSet *)
module BlockMap = Map.Make(struct
  type t = label
  let compare = String.compare
end)

let blk2label (b:block):label = 
  match b with
  | Label(lb)::_ -> lb
  | _ -> raise Implement_Me

let upd_bmap key item mp =
  let set = BlockMap.find_opt key mp in
  let updset = match set with
  | Some set -> LabelSet.add item set
  | None -> LabelSet.add item LabelSet.empty
   in
  BlockMap.add key updset mp
  
let rec live_eq f mapa mapb = 
  match f with
  | blk::res ->
    let mapa_set = LiveMap.find (blk2label blk) mapa in
    let mapb_set = LiveMap.find (blk2label blk) mapb in
    (* if VarSet.is_empty (VarSet.diff mapa_set mapb_set) then *)
    if VarSet.equal mapa_set mapb_set then
      live_eq res mapa mapb
    else
      false
  | _ ->true

let rec create_emp_livemap f map= 
  match f with 
  | blk::res -> 
    let upd_map = LiveMap.add (blk2label blk) VarSet.empty map in
    create_emp_livemap res upd_map
  | _ -> map

let rec create_emp_blockmap f map= 
  match f with 
  | blk::res -> 
    let upd_map = BlockMap.add (blk2label blk) LabelSet.empty map in
    create_emp_blockmap res upd_map
  | _ -> map

exception NodeValueNotSupport
let opd2gnode opd = 
  match opd with
  | Reg(r) -> RegNode(r)
  | Var(v) -> VarNode(v)
  | Lab(v) -> VarNode(v)
  | _ -> raise NodeValueNotSupport

let save_rm (e:operand) (vset:VarSet.t) = 
  match VarSet.find_opt e vset with
  | Some _ -> VarSet.remove e vset
  | None -> vset
 
let build_interfere_graph (f : func) : interfere_graph = 
    (* func, inmp, outmp -> (updinmp,updoutmp) *)
    let rec create_ioblocks (f:func) (inmp: (LabelSet.t) BlockMap.t) (outmp: (LabelSet.t) BlockMap.t): ((LabelSet.t) BlockMap.t * (LabelSet.t) BlockMap.t) = 
      let rec create_ioblock (lbname:label)(b:block) (inmp: (LabelSet.t) BlockMap.t) (outmp: (LabelSet.t) BlockMap.t): ((LabelSet.t) BlockMap.t * (LabelSet.t) BlockMap.t) = 
        match b with
        | [blk] -> 
          let upd_outmap = 
            (match blk with
            | Jump(to_lb)->
              upd_bmap lbname to_lb outmp
            | If(_,_,_,to_lb1,to_lb2) ->
              let temp_map = upd_bmap lbname to_lb1 outmp in
              upd_bmap lbname to_lb2 temp_map
            | Return -> outmp
            |_ -> raise Implement_Me) in 
          let upd_inmap = 
            (match blk with
            | Jump(to_lb)->
              upd_bmap to_lb lbname inmp
            | If(_,_,_,to_lb1,to_lb2) ->
              let temp_map = upd_bmap to_lb1 lbname inmp in
              upd_bmap to_lb2 lbname temp_map
            | Return -> inmp
            |_ -> raise Implement_Me) in 
          (upd_inmap,upd_outmap)
        | _::res -> create_ioblock lbname res inmp outmp
        | _ -> raise Implement_Me
      in
      match f with
      | blk::res -> (
        let (upd_inmp,upd_outmp) = (create_ioblock (blk2label blk) blk inmp outmp) in
        create_ioblocks res upd_inmp upd_outmp
      )
      | [] -> (inmp,outmp)
    in
    let rec get_k_g (b:block) (k: VarSet.t)(g:VarSet.t):(VarSet.t*VarSet.t) =  
      match b with
      | i::res -> (
        let (upd_k,upd_g) = 
          match i with
          | Move(x,y) -> (
            match y with 
            | Var(_) | Reg(_) | Lab(_)-> (VarSet.add x (save_rm y k),VarSet.add y (save_rm x g))
            | _ -> (VarSet.add x k,(save_rm x g))
          )
          | Arith(x,y,_,z) -> 
            let (upd_k,upd_g) = 
              match y with 
              | Var(_) | Reg(_) | Lab(_)-> save_rm y k,VarSet.add y g
              | _ -> k,g
            in
            let (upd_k,upd_g) = 
              match z with 
              | Var(_) | Reg(_) | Lab(_)-> save_rm z upd_k,VarSet.add z upd_g
              | _ -> upd_k,upd_g
            in (VarSet.add x k,save_rm x upd_g)
          | Load(x,y,_) -> (VarSet.add x (save_rm y k),VarSet.add y (save_rm x g))
          | Store(x,_,y) -> (VarSet.add x (save_rm y k),VarSet.add y (save_rm x g))
          | Call(x) -> (VarSet.add x k,(save_rm x g)) (* TODO: not sure*)
          | If(x,_,y,_,_) -> 
            let (upd_k,upd_g) = 
              match x with 
              | Var(_) | Reg(_) | Lab(_)-> save_rm x k,VarSet.add x g
              | _ -> save_rm x k,g
            in
            let (upd_k,upd_g) = 
              match y with 
              | Var(_) | Reg(_) | Lab(_)-> save_rm y upd_k,VarSet.add y upd_g
              | _ -> save_rm y upd_k,upd_g
            in (upd_k,upd_g)
          | _ -> (k,g)
        in (get_k_g res upd_k upd_g)
      )
      | _ -> (k,g)
    in
    (*f,kmp,gmp -> fullkmp,fullgmp*)
    let rec get_k_g_blocks (f:func) (kmp: (VarSet.t) GKMap.t) (gmp: (VarSet.t) GKMap.t):((VarSet.t) GKMap.t*(VarSet.t) GKMap.t) = 
      match f with 
      | blk::res -> 
        let (kset,gset) = (get_k_g (List.rev blk) VarSet.empty VarSet.empty) in
        let lb = (blk2label blk) in
        let (upd_kmap,upd_gmap) = ((GKMap.add lb kset kmp),(GKMap.add lb gset gmp)) in
        (get_k_g_blocks res upd_kmap upd_gmap)
      | _ -> (kmp,gmp)
    in
    (*f,liveinmap,liveoutmap,killmap, genmap, blockinmap, blockoutmap -> liveinmap,liveoutmap*)
    (*one iteration*)
    (*since use block list to iterate, have extra logn for lookup block in map
       Map.fold can remove logn*)
    (*need inmp initilization*)
    let rec create_iolives (f:func) (inmp: (VarSet.t) LiveMap.t) (outmp: (VarSet.t) LiveMap.t)
            (killmp: (VarSet.t) GKMap.t)(genmp: (VarSet.t) GKMap.t)(binmap: (LabelSet.t) BlockMap.t)(boutmap: (LabelSet.t) BlockMap.t): 
            ((VarSet.t) LiveMap.t * (VarSet.t) LiveMap.t) = 

      let rec create_iolive (lbname:label)(b:block) (inmp: (VarSet.t) LiveMap.t) (outmp: (VarSet.t) LiveMap.t)
              (killmp: (VarSet.t) GKMap.t)(genmp: (VarSet.t) GKMap.t)(binmap: (LabelSet.t) BlockMap.t)(boutmap: (LabelSet.t) BlockMap.t): 
              ((VarSet.t) LiveMap.t * (VarSet.t) LiveMap.t) = 
        let succ = BlockMap.find lbname boutmap in
        let union_func (blkname:label) (liveinset:VarSet.t) = 
          VarSet.union liveinset (LiveMap.find blkname inmp)
        in
        let succ_livein_union = LabelSet.fold (union_func) succ VarSet.empty in
        (* let self_inset = LiveMap.find lbname inmp in *)
        let self_outset = LiveMap.find lbname outmp in
        let (upd_inmp,updoutmp) = 
          (*check the diff order: succ>=selfout*)
          (* (if VarSet.is_empty (VarSet.diff succ_livein_union self_outset) then *)
          (if VarSet.equal succ_livein_union self_outset then
            (inmp,outmp)
          else
            let upd_outset = succ_livein_union in
            let self_genset = GKMap.find lbname genmp in
            let self_killset = GKMap.find lbname killmp in
            let upd_inset = VarSet.union self_genset (VarSet.diff succ_livein_union self_killset) in
            (LiveMap.add lbname upd_inset inmp,LiveMap.add lbname upd_outset outmp))
        in (upd_inmp,updoutmp)
            
      in
      match f with
      | blk::res -> (
        let (upd_inmp,upd_outmp) = (create_iolive (blk2label blk) blk inmp outmp killmp genmp binmap boutmap) in
        create_iolives res upd_inmp upd_outmp killmp genmp binmap boutmap
      )
      | [] -> (inmp,outmp)
    in
    (* print_endline "start creation"; *)
    let init_block_inmap = create_emp_blockmap f BlockMap.empty in
    let init_block_outmap = create_emp_blockmap f BlockMap.empty in
    let (binmap,boutmap) = create_ioblocks f init_block_inmap init_block_outmap in
    (* print_endline "blockmap complete"; *)
    (* (BlockMap.iter (fun key value -> print_endline key;(LabelSet.iter (fun var -> print_string (var^" ")) value);print_endline "") binmap);
    print_endline "outmap:";
    (BlockMap.iter (fun key value -> print_endline key;(LabelSet.iter (fun var -> print_string (var^" ")) value);print_endline "") boutmap); *)
    (* (BlockMap.iter (fun key _ -> print_endline key) binmap); *)
    (* (BlockMap.iter (fun key _ -> print_endline key) boutmap); *)
    let (killmap,genmap) = get_k_g_blocks f GKMap.empty GKMap.empty in
    (*initialize liveinmap*)
    (* print_endline "killgenmap complete"; *)
    let init_livein_map = genmap in
    let init_liveout_map = create_emp_livemap f LiveMap.empty in
    (* (LiveMap.iter (fun key _ -> print_endline key) init_livein_map); *)
    (* (LiveMap.iter (fun key _ -> print_endline key) init_liveout_map); *)
    let rec make_livemap old_livein_map old_liveout_map = 
      let (new_inmap,new_outmap) = 
        (create_iolives f old_livein_map old_liveout_map killmap genmap binmap boutmap)
      in
      (if (live_eq f new_inmap old_livein_map) && (live_eq f new_outmap old_liveout_map) then
        (new_inmap,new_outmap)
      else
        (make_livemap new_inmap new_outmap))
    in
    let (liveinmap, liveoutmap) = make_livemap init_livein_map init_liveout_map in
    (* print_endline "kill map:";
    (LiveMap.iter (fun key value -> print_endline key;(VarSet.iter (fun var -> print_string ((op2string var)^" ")) value);print_endline "") killmap);
    print_endline "livein map:";
    print_endline "livemap complete";
    print_endline "init_livein map:";
    (LiveMap.iter (fun key value -> print_endline key;(VarSet.iter (fun var -> print_string ((op2string var)^" ")) value);print_endline "") init_livein_map);
    print_endline "livein map:";
    (LiveMap.iter (fun key value -> print_endline key;(VarSet.iter (fun var -> print_string ((op2string var)^" ")) value);print_endline "") liveinmap);
    print_endline "liveout map:";
    (LiveMap.iter (fun key value -> print_endline key;(VarSet.iter (fun var -> print_string ((op2string var)^" ")) value);print_endline "") liveoutmap); *)
    (* raise Implement_Me *)


    let rec make_graph f g (liveoutmap:VarSet.t LiveMap.t) = 
      let rec update_graph b g (vset:VarSet.t)= 
        let addedge_opd_set (opd:operand) (vset:VarSet.t) g = 
          let addone (opd1:operand) (opd2:operand) g = 
            specialAddEdge (opd2gnode opd1) (opd2gnode opd2) g
          in
          VarSet.fold (fun x g -> (addone opd x g)) vset g
        in
        match b with
        | i::res -> (
          let (upd_graph,upd_vset) = 
            match i with
            | Move(x,y) -> (
              match y with 
              | Var(_) | Reg(_) | Lab(_)-> 
                (addedge_opd_set x vset g,
                VarSet.add y (VarSet.remove x vset))
              | _ -> 
                (addedge_opd_set x vset g,
                (VarSet.remove x vset))
            )
            | Arith(x,y,_,z) -> 
              (addedge_opd_set x vset g,
              let upd_vset = 
                match y with 
                | Var(_) | Reg(_) | Lab(_)->  VarSet.add y vset
                | _ -> vset
              in
              let upd_vset = 
                match z with 
                | Var(_) | Reg(_) | Lab(_)-> VarSet.add z upd_vset
                | _ -> upd_vset
              in (VarSet.remove x upd_vset))
            | Load(x,y,_) -> (addedge_opd_set x vset g,
                VarSet.add y (VarSet.remove x vset))
            | Store(x,_,y) -> (addedge_opd_set x vset g,
                VarSet.add y (VarSet.remove x vset))
            | Call(x) -> (g,
                VarSet.add x vset) (* TODO: not sure*)
            | If(x,_,y,_,_) -> 
              let upd_vset = 
                match x with 
                | Var(_) | Reg(_) | Lab(_)-> (VarSet.add x vset)
                | _ -> vset
              in
              let upd_vset = 
                match y with 
                | Var(_) | Reg(_) | Lab(_)-> (VarSet.add y upd_vset)
                | _ -> upd_vset
              in (g,upd_vset)
            | _ -> (g,vset)
          in (update_graph res upd_graph upd_vset)
        )
        | _ -> (g,vset)
      in
      match f with
      | blk::res -> 
        let (upd_g,_) = update_graph (List.rev blk) g (LiveMap.find (blk2label blk) liveoutmap) in
        make_graph res upd_g liveoutmap

      | _ -> g
    in
    make_graph f IUGraph.empty liveoutmap
