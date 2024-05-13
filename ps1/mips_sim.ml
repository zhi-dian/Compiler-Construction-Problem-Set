open Mips_ast
open Mips_assem
open Byte

exception TODO
exception FatalError



(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
 *)
let rec interp (init_state : state) : state = 
  let init_reg = init_state.r in
  let init_pc = init_state.pc in 
  let init_mem = init_state.m in
  let getint = fun x -> match x with 
  | 6 -> 63l | 5 -> 31l | 26 -> 67108863l | 16 -> 65535l | _ -> 0l in
  let word2instformat (word:int32) :instformat = 
    match Int32.logand (Int32.shift_right word (32-6)) (getint 6) with
    | 0l -> 
      (let rd = Int32.logand (Int32.shift_right word (32-21)) (getint 5) in 
      let rs = Int32.logand (Int32.shift_right word (32-11)) (getint 5) in 
      let rt = Int32.logand (Int32.shift_right word (32-16)) (getint 5) in 
      match Int32.logand (Int32.shift_right word (32-32)) (getint 6) with
      | 0x20l -> 
        R {r_opcode=0l;r_rs=rs;r_rt=rt;r_rd=rd;
        r_shamt=0l;r_fun=0x20l}
      | 0x8l -> 
        R {r_opcode=0l;r_rs=rs;r_rt=0l;r_rd=0l;r_shamt=0l;r_fun=0x8l}
      | _ -> raise FatalError)
    | 0x3l -> J {j_opcode=0x3l;j_addr=Int32.logand (Int32.shift_right word (32-32)) (getint 26)}
    | opcode -> 
      (let rs = Int32.logand (Int32.shift_right word (32-11)) (getint 5) in 
      let rt = Int32.logand (Int32.shift_right word (32-16)) (getint 5) in  
      let imm = Int32.logand (Int32.shift_right word (32-32)) (getint 16) in
      match opcode with 
      | 0x4l -> I {i_opcode=0x4l;i_rs= rs;i_rt= rt;i_imm=imm}
      | 0xfl -> I {i_opcode=0xfl;i_rs=0l;i_rt= rt;i_imm=imm}
      | 0xdl -> I {i_opcode=0xdl;i_rs= rs;i_rt= rt;i_imm=imm}
      | 0x23l -> I {i_opcode=0x23l;i_rs= rs;i_rt= rt;i_imm=imm}
      | 0x2bl -> I {i_opcode=0x2bl;i_rs= rs;i_rt= rt;i_imm=imm}
      | _ -> raise FatalError
    
      ) in
  match (rev_endianess (read_word init_mem init_pc)) with
  | 0l -> print_endline "stop";{r=init_reg;pc=init_pc;m=init_mem}
  | word' -> 
  let ins = instformat2ins(word2instformat(word')) in
  print_endline (inst2str ins);
  (* print_endline (Int32.to_string (instformat2word (ins2instformat (instformat2ins(word2instformat(word'))))));
  print_endline (Int32.to_string (instformat2word (word2instformat(word'))));
  print_endline (Int32.to_string word');
  print_endline (Int32.to_string (instformat2word (ins2instformat (Add (R3,R3,R3))))); 
  print_endline (inst2str (Add (R3, R3, R3)));
  print_endline (inst2str (instformat2ins (ins2instformat (Add (R3, R3, R3))))); *)
  
  (match ins with 
  | Add (rd, rs, rt) -> 
    let sum' = Int32.add (rf_lookup (reg2ind rs) init_reg) (rf_lookup (reg2ind rt) init_reg) in
    let new_r = rf_update (reg2ind rd) sum' (init_reg) in
    interp {r=new_r;pc= Int32.add init_pc 4l;m=init_mem}
  | Beq (rs, rt, offset) ->
    let v1 = rf_lookup (reg2ind rs) init_reg in
    let v2 = rf_lookup (reg2ind rt) init_reg in
    if v1 = v2 then 
      interp {r=init_reg;pc=Int32.add init_pc (Int32.shift_left offset 2);m=init_mem} else 
      interp {r=init_reg;pc=Int32.add init_pc 4l;m=init_mem}
  | Jr rs ->
    interp {r=init_reg;pc=(rf_lookup (reg2ind rs) init_reg);m=init_mem}
  | Jal addr -> 
    let new_r =  rf_update 31 (Int32.add init_pc 4l) (init_reg) in
    let new_pc = Int32.logor (Int32.shift_left addr 2 ) (Int32.logand init_pc 0xF000000l) in
    interp {r=new_r;pc=new_pc;m=init_mem}
  | Li (rd, imm) -> raise (PError "Li encountered")
  | Lui (rt, imm) ->
    let rst = Int32.shift_left imm 16 in
    let new_r = rf_update (reg2ind rt) rst (init_reg) in
    interp {r=new_r;pc=Int32.add init_pc 4l;m=init_mem}
  | Ori (rt, rs, imm) ->
    let v1 = rf_lookup (reg2ind rs) init_reg in
    let rst = Int32.logor v1 imm in
    let new_r = rf_update (reg2ind rt) rst (init_reg) in
    interp {r=new_r;pc=Int32.add init_pc 4l;m=init_mem}
  | Lw (rt, rs, imm) ->
    let v1 = rf_lookup (reg2ind rs) init_reg in
    let rst = rev_endianess(read_word init_mem (Int32.add v1 imm)) in
    let new_r = rf_update (reg2ind rt) rst (init_reg) in
    interp {r=new_r;pc=Int32.add init_pc 4l;m=init_mem}
  | Sw (rs, rt, imm) ->
    let v1 = rf_lookup (reg2ind rt) init_reg in
    let v2 = rf_lookup (reg2ind rs) init_reg in
    let curWord = v2 in
    let curPos = Int32.add v1 imm in
    (* let v1 = rf_lookup (reg2ind rt) init_reg in
    let rst = read_word init_mem imm in
    let curWord = Int32.add v1 rst in
    let curPos = imm in *)
    let new_mem = 
      mem_update (curPos) (getByte curWord  0) (
      mem_update (Int32.add curPos 1l) (getByte curWord 1) (
      mem_update (Int32.add curPos 2l) (getByte curWord 2) (
      mem_update (Int32.add curPos 3l) (getByte curWord 3) (
    init_mem)))) in
    interp {r=init_reg;pc=Int32.add init_pc 4l;m=new_mem})



(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Big Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Stack grows downward starting at 0x7ffffffc
  * > GP points to 30000000
  * > The assembler uses register 1 as temp storage for encoding Li
  * > We don't implement delay slots in either assembly or bitcode semantics
  * > As stated in lecture, we shift jump and break immediates left by 2
  * > The PC is always incremented before executing an instruction
  * > Beq subtracts 4 from the PC before adding its offset
  * > We preserve the top 4 bits of the PC when executing a jal
*)
