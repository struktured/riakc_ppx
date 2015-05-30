module Log = struct
  open Core.Std;;
  open Async.Std;;

  (*val hex_of_string : 'a -> (?sep:Core.Std.String.t -> Core.Std.String.t -> Core.Std.String.t) Async_kernel.Deferred.t = <fun>  
    Note: Always print 2 chars...do not elide leading zero else hex in unreadable. *)
  let hex_of_string_monadic s =
    return s >>| (fun _ -> Core.Std.String.concat_map ~f:(fun c -> Core.Std.sprintf "%02X" (Core.Std.Char.to_int c)));;
  let hex_of_string s =
    Core.Std.String.concat_map s ~f:(fun c -> Core.Std.sprintf "%02X" (Core.Std.Char.to_int c));;    

  (*Core.Std.Out_channel.t option Core.Std.String.Map.t*)
  let mapofoutchans = Core.Std.String.Map.empty;;
  let reftoMap = ref mapofoutchans;;
  let getOutchan logname =
    let searchresult = Core.Std.String.Map.find !reftoMap logname in
    match searchresult with 
    | Some oc -> oc;
    | None -> let newoutchan = Core.Std.Out_channel.create logname ~binary:false ~append:true in 
	      reftoMap := Core.Std.String.Map.add !reftoMap logname newoutchan;
	      newoutchan;;
  (*val getOutchan :  Core.Std.String.Map.Key.t -> Core.Std.Out_channel.t Async_kernel.Deferred.t =  <fun>*)
  let getOutchan_monadic logname =  
    let f searchresult = 
      match searchresult with 
      | Some oc -> return oc;
      | None -> return (Core.Std.Out_channel.create logname ~binary:false ~append:true) >>=
	(fun newoutchan -> reftoMap := Core.Std.String.Map.add !reftoMap logname newoutchan; return (newoutchan)) in
    return (logname) >>| (fun oc -> Core.Std.String.Map.find !reftoMap oc) >>= f;;
  (*val closeAll : unit -> unit Async_kernel.Deferred.t = <fun>*)
  let closeAll () = 
    let f ~(key:Core.Std.String.Map.Key.t) ~data =
      let _ = key in Core.Std.Out_channel.close data in
    return (()) >>= (fun _ -> Core.Std.String.Map.iter !reftoMap f; return(())) >>| (fun _ -> reftoMap := Core.Std.String.Map.empty);;
  (*val noteFunction_monadic :
  Core.Std.String.Map.Key.t -> string -> unit Async_kernel.Deferred.t = <fun> SUBBING THIS FUNC FOR NEXT BY RENAME *)
  let noteFunction logname fname =
    getOutchan_monadic logname >>| (fun oc -> (Core.Std.Out_channel.output_string oc ("\n"^ fname ^"()\n"); Core.Std.Out_channel.flush oc; oc));;
  (*val noteFunction_old : Core.Std.String.Map.Key.t -> string -> unit = <fun>
  let noteFunction logname fname = 
    let oc = getOutchan logname in
    let _ = Core.Std.Out_channel.output_string oc ("\n"^ fname ^"() \n") in
    Core.Std.Out_channel.flush oc;;*)
  (*val generalLog :  Core.Std.String.Map.Key.t -> string -> unit Async_kernel.Deferred.t = <fun>*)
  let generalLog logname message =
    getOutchan_monadic logname >>| (fun oc -> (Core.Std.Out_channel.output_string oc (message ^ "\n"); Core.Std.Out_channel.flush oc;));;
(*===========================Supporting functions for this terrible logging module===============================================================*)
(*val document :
  Core.Std.String.Map.Key.t ->
  string ->
  Core.Std.String.t option -> Core.Std.String.t option -> string -> unit =
  <fun>*)
let document logname bucket key vclock putorget = 
  let outchan = getOutchan logname in
  let _ = Core.Std.Out_channel.output_string outchan ("\n" ^ putorget ^ " p.bucket:" ^ bucket ^ "\n") in 
  match key, vclock with
    Some k, Some v -> let _ = Core.Std.Out_channel.output_string outchan (" p.key:" ^ k ^ "vclock:" ^ v ^ "\n") in
		      Core.Std.Out_channel.flush outchan;
  | Some k, None -> let _ = Core.Std.Out_channel.output_string outchan (" p.key:" ^ k ^ " vclock is None \n") in
		    Core.Std.Out_channel.flush outchan;
  | None, Some v -> let _ = Core.Std.Out_channel.output_string outchan (" p.key is None p.vclock:" ^ v ^ "\n") in
		    Core.Std.Out_channel.flush outchan;
  | None, None -> let _ = Core.Std.Out_channel.output_string outchan (" p.key is None   p.vclock is None \n") in 
		  Core.Std.Out_channel.flush outchan;;
(*val document :   
  Core.Std.String.Map.Key.t ->
  string ->
  Core.Std.String.t option -> 
  Core.Std.String.t option -> string -> unit Async_kernel.Deferred.t = <fun>*)
let document_monadic logname bucket key vclock putorget = 
  let f outchan key vclock = 
    match key, vclock with
      Some k, Some v -> let _ = Core.Std.Out_channel.output_string outchan (" (HEX) p.key:" ^ (hex_of_string k) ^ "vclock(HEX):" ^ (hex_of_string v) ^ "\n") in
			Core.Std.Out_channel.flush outchan;
    | Some k, None -> let _ = Core.Std.Out_channel.output_string outchan (" p.key(HEX):" ^ (hex_of_string k) ^ " vclock is None \n") in
		      Core.Std.Out_channel.flush outchan;
    | None, Some v -> let _ = Core.Std.Out_channel.output_string outchan (" p.key is None p.vclock(HEX):" ^ (hex_of_string v) ^ "\n") in
		      Core.Std.Out_channel.flush outchan;
    | None, None -> let _ = Core.Std.Out_channel.output_string outchan (" p.key is None   p.vclock is None \n") in 
		    Core.Std.Out_channel.flush outchan in
  getOutchan_monadic logname >>| (fun outchan -> (Core.Std.Out_channel.output_string outchan ("\n" ^ putorget ^ " p.bucket:" ^ bucket)); outchan) >>| f >>| (fun f2 -> f2 key) >>| (fun f3 -> f3 vclock);;
let docint32 logname pi sname tag = 
  let outchan = getOutchan logname in
  let t = string_of_int tag in
  match pi with
    Some i -> let s = Int32.to_string i in 
	      let _ = Core.Std.Out_channel.output_string outchan (" " ^ sname ^ "  Int32: " ^ s ^ " at tag:" ^ t ^ "\n") in
	      Core.Std.Out_channel.flush outchan;
  | None -> let s = "32Int opt is None at tag:" ^ t ^ "\n"  in  
	    let _ = Core.Std.Out_channel.output_string outchan s in 
	    Core.Std.Out_channel.flush outchan;;
let docint32_monadic logname pi sname tag =
  let f outchan pi = 
    match pi with
      Some i -> let s = Int32.to_string i in 
		let _ = Core.Std.Out_channel.output_string outchan (" " ^ sname ^ "  Int32: " ^ s ^ " at tag:" ^ (string_of_int tag) ^ "\n") in
		Core.Std.Out_channel.flush outchan;
    | None -> let s = "32Int opt is None at tag:" ^ (string_of_int tag) ^ "\n"  in  
	      let _ = Core.Std.Out_channel.output_string outchan s in 
	      Core.Std.Out_channel.flush outchan in
  getOutchan_monadic logname >>| (fun outchan -> f outchan) >>| (fun f2 -> f2 pi);;
let docint64_monadic logname pi tag =
  let f outchan pi =
    match pi with
    | Some i -> let s = Int64.to_string i in 
		let _ = Core.Std.Out_channel.output_string outchan ("Int64: " ^ s ^ " at tag:" ^ (string_of_int tag) ^ "\n") in 
		Core.Std.Out_channel.flush outchan;
    | None -> let s = "64Int opt is None at tag:" ^ (string_of_int tag) ^ "\n" in 
	      let _ = Core.Std.Out_channel.output_string outchan s in 
	      Core.Std.Out_channel.flush outchan in
  getOutchan_monadic logname >>| (fun outchan -> f outchan) >>| (fun f2 -> f2 pi);;
let docint64 logname pi tag = 
  let t = string_of_int tag in
  let outchan = getOutchan logname in
  match pi with
  | Some i -> let s = Int64.to_string i in 
	      let _ = Core.Std.Out_channel.output_string outchan ("Int64: " ^ s ^ " at tag:" ^ t ^ "\n") in 
	      Core.Std.Out_channel.flush outchan;
  | None -> let s = "64Int opt is None at tag:" ^ t ^ "\n" in 
	    let _ = Core.Std.Out_channel.output_string outchan s in 
	    Core.Std.Out_channel.flush outchan;;
let doc_bytes_opt logname b s tag = 
  let outchan = getOutchan logname in 
  let t = string_of_int tag in
  match b with
    Some v -> let _ = Core.Std.Out_channel.output_string outchan (" bytes(HEX) of " ^ s ^ ":" ^ (hex_of_string v) ^ "\n") in
	      Core.Std.Out_channel.flush outchan;
  | None -> let _ = Core.Std.Out_channel.output_string outchan (" bytes of " ^ s^ ": NONE at tag:" ^ t ^ "\n") in
	    let _ = Core.Std.Out_channel.output_string outchan (" bytes(HEX): NONE at tag:" ^ t ^ "\n") in
	      Core.Std.Out_channel.flush outchan;;
let doc_bytes_opt_monadic logname b s tag = 
  let f outchan b s tag = 
    match b with
      Some v -> let _ = Core.Std.Out_channel.output_string outchan (" bytes(HEX) of " ^ s ^ ":" ^ (hex_of_string v) ^ "\n") in
		Core.Std.Out_channel.flush outchan;
    | None -> let _ = Core.Std.Out_channel.output_string outchan (" bytes of " ^ s^ ": NONE at tag:" ^ (string_of_int tag) ^ "\n") in
	      let _ = Core.Std.Out_channel.output_string outchan (" bytes(HEX): NONE at tag:" ^ (string_of_int tag) ^ "\n") in
	      Core.Std.Out_channel.flush outchan in
  getOutchan_monadic logname >>| (fun outchan -> f outchan) >>| (fun f2 -> f2 b) >>| (fun f3 -> f3 s) >>| (fun f4 -> f4 tag);;
let doc_bytes logname b s tag = 
  let outchan = getOutchan logname in
  let t = string_of_int tag in
  let _ = Core.Std.Out_channel.output_string outchan (" bytes(HEX) of " ^ s ^ ":" ^ (hex_of_string b) ^ " at Tag:" ^ t ^ "\n") in
  Core.Std.Out_channel.flush outchan;;

let doc_bytes_monadic logname b s tag = 
  let f outchanb s tag  = Core.Std.Out_channel.output_string outchanb (" bytes(HEX) of " ^ s ^ ":" ^ (hex_of_string b) ^ " at Tag:" ^ (string_of_int tag) ^ "\n"); Core.Std.Out_channel.flush outchanb in
  getOutchan_monadic logname >>| (fun outchan -> f outchan) >>| (fun f2 -> f2 s) >>| (fun f3 -> f3 tag);;

let doc_bool_opt logname b s tag = 
  let outchan = getOutchan logname in 
  let t = string_of_int tag in
  match b with 
  | Some v -> if v then let s = "Bool opt is true: " ^ s ^ " at tag:" ^ t ^ "\n" in
			let _ = Core.Std.Out_channel.output_string outchan s in
			Core.Std.Out_channel.flush outchan;
    else let s = "Bool opt is false: " ^ s ^ " at tag: " ^ t ^ "\n" in
	 let _ = Core.Std.Out_channel.output_string outchan s in
         Core.Std.Out_channel.flush outchan;
  | None -> let s = "Bool opt is None: " ^ s ^ " at tag:" ^ t ^ "\n" in 
	    let _ = Core.Std.Out_channel.output_string outchan s in 
	    Core.Std.Out_channel.flush outchan;;

let doc_bool_opt_monadic logname b s tag = 
  let f outchanb s t = 
    match b with 
    | Some v -> if v then Core.Std.Out_channel.output_string outchanb ("Bool opt is True:" ^ s ^ " at tag:" ^ (string_of_int t) ^ "\n") else Core.Std.Out_channel.output_string outchanb ("Bool opt is False: " ^ s ^ " at tag: " ^ (string_of_int t) ^ "\n")
    | None -> Core.Std.Out_channel.output_string outchanb ("Bool opt is None: " ^ s ^ " at tag:" ^ (string_of_int t) ^ "\n"); Core.Std.Out_channel.flush outchanb in
  getOutchan_monadic logname >>| (fun outchan -> f outchan) >>| (fun f2 -> f2 s) >>| (fun f4 -> f4 tag);;

let doc_bool logname b s tag = 
  let outchan = getOutchan logname in 
  let t = string_of_int tag in
  match b with 
  | true -> let s = "Bool is true: " ^ s ^ " at tag:" ^ t ^ "\n" in
	    let _ = Core.Std.Out_channel.output_string outchan s in
	    Core.Std.Out_channel.flush;
  | false -> let s = "Bool is false: " ^ s ^ " at tag: " ^ t ^ "\n" in
	     let _ = Core.Std.Out_channel.output_string outchan s in
             Core.Std.Out_channel.flush;;
let doc_s logname ps tag = 
  let outchan = getOutchan logname in 
  let t = string_of_int tag in
  match ps with
    Some s ->  let _ = Core.Std.Out_channel.output_string outchan ("string opt: " ^ s ^ " at tag:" ^ t ^ "\n") in
	       Core.Std.Out_channel.flush outchan;
  | None -> let s = "String opt is None at tag:" ^ t ^ "\n" in 
	    let _ = Core.Std.Out_channel.output_string outchan s in 
	    Core.Std.Out_channel.flush outchan;;

(*TODO: logging for list for use with list keys*)
let doc_embeddedmsg logname content name tag =
  let outchan = getOutchan logname in 
  let t = string_of_int tag in  
  let tooutput = "Entire embd msg:" ^ content ^ " and HEX entire embd msg:" ^ (hex_of_string content) ^ ", " ^ name ^ ", at tag:" ^ t in
  let _ = Core.Std.Out_channel.output_string outchan tooutput in
  Core.Std.Out_channel.flush outchan;;
end
