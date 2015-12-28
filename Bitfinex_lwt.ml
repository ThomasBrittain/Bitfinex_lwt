(* by: Thomas Brittain <tdbrittain86@gmail.com> *)
(* An Lwt friendly library for the Bitfinex API *)

(* To Compile Manually: *)
(* ocamlfind ocamlc -c -o Bitfinex_lwt *)
(* -thread -syntax camlp4o *)
(* -package yojson,lwt,lwt.syntax,cohttp.lwt,hex,nocrypto *)
(* -linkpkg Bitfinex_lwt.ml *)

(* TODO: Incorporate SSL into post_it and get_it *)

open Yojson.Basic.Util

module type AccountInfo =
  sig
    val public_key : string
    val private_key : string
  end

module Make (U : AccountInfo) = struct

  let (>>=) = Lwt.bind

  let public_key = U.public_key
  let private_key = U.private_key

  let bitfinex_api = "https://api.bitfinex.com/v1/"

  (* Post request *)
  let post_it path base64_payload encrypted_signature =
    let post_uri = Uri.of_string (bitfinex_api ^ path) in
    let post_header =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "X-BFX-APIKEY" public_key
      |> fun h -> Cohttp.Header.add h "X-BFX-PAYLOAD" base64_payload
      |> fun h -> Cohttp.Header.add h "X-BFX-SIGNATURE" encrypted_signature
    in
    Cohttp_lwt_unix.Client.post (*~ctx ~body ~chunked*) ~headers:post_header post_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >>= fun s -> Yojson.Basic.from_string s |> Lwt.return

  (* Get request *)
  let get_it path =
    let get_uri = Uri.of_string (bitfinex_api ^ path) in
    Cohttp_lwt_unix.Client.get get_uri
    >>= fun (a, b) -> b |> Cohttp_lwt_body.to_string
    >>= fun s -> Yojson.Basic.from_string s |> Lwt.return

  (* Insturment type *)
  type inst = USD | BTC | LTC | USDReturnSwap | BTCReturnSwap | LTCReturnSwap | InstError

  (* Wallet kind *)
  type wallet_kind = Trading | Deposit | Exchange | WalletKindError

  (* Wallet *)
  type wallet = {
    wallet_kind : wallet_kind;
    instrument : inst;
    amount : float;
    available : float
  }

  (* Ticker type *)
  type ticker = {
    mid : float;
    bid : float;
    ask : float;
    last_price : float;
    low : float;
    high : float;
    volume : float;
    ticker_time : float
  }

  (* Lendbook type *)
  type lendbook = {
    rate : float;
    amount : float;
    period : int;
    timestamp : UnixLabels.tm;
    frr : bool
  }

  (* Orderbook type *)
  type orderbook = {price : float; amount : float; timestamp : UnixLabels.tm}

  (* Trade type *)
  type trade = {price : float; amount : float; timestamp : UnixLabels.tm}

  (* Symbol type *)
  type symbol = BtcUsd | LtcUsd | LtcBtc | DrkUsd | DrkBtc | Th1Btc | SymbolError

  (* Position type *)
  type position_type = Long | Short

  (* Order kind *)
  type order_kind = Buy | Sell | Short | Cover | OrderKindError

  (* Order type *)
  (* NOTE: Types starting with "Exchange" are exchange orders, others are margin trading orders *)
  type order_type = Limit
                  | Market
                  | Stop
                  | TrailingStop
                  | FillOrKill
                  | ExchangeMarket
                  | ExchangeLimit
                  | ExchangeStop
                  | ExchangeTrailingStop
                  | ExchangeFillOrKill
                  | OrderTypeError

  (* Order *)
  type order = {
    order_kind : order_kind;
    order_type : order_type;
    symbol : symbol;
    price : float;
    amount : float;
    stop_loss : float;
    take_profit : float;
    is_hidden : bool
  }

  (* Order Receipt *)
  type order_receipt = {
    order_id : int;
    symbol : symbol;
    exchange : string;
    price : float;
    avg_execution_price : float;
    side : order_kind;
    order_type : order_type;
    timestamp : Unix.tm;
    is_live : bool;
    is_cancelled : bool;
    is_hidden : bool;
    was_forced : bool;
    original_amount : float;
    remaining_amount : float;
    executed_amount : float
  }

  (* Position *)
  (* TODO: Find out what all the possible status' are and make a status type.*)
  (*       Then incorpoarte below. *)
  type position = {
    id : int;
    symbol : symbol;
    status : string;
    base : float;
    amount : float;
    timestamp : Unix.tm;
    swap : float;
    pl : float
  }

  (* Balance History *)
  type balance_history = {
    symbol : symbol;
    amount : float;
    balance : float;
    description : string;
    timestamp : Unix.tm
  }

  (* Past Trade Record *)
  type past_trade = {
    price : float;
    amount : float;
    timestamp : Unix.tm;
    exchange : string;
    order_type : order_type;
    fee_currency : inst;
    fee_amount : float;
    tid : int;
    order_id : int
  }

  (* TODO: Change Failure to Failure of string so that messages can be printed with a Failure *)
  (* Order Result type *)
  type order_result = Success of order_receipt | MultiOrderSuccess | Failure

  let string_of_inst inst =
      match inst with
        | USD -> "usd"
        | BTC -> "btc"
        | LTC -> "ltc"
        | USDReturnSwap -> "UsdReturnSwap"
        | BTCReturnSwap -> "BtcReturnSwap"
        | LTCReturnSwap -> "LtcReturnSwap"
        | InstError -> "InstError"

  let inst_of_string s =
      match s with
        | "usd" -> USD
        | "btc" -> BTC
        | "ltc" -> LTC
        | "UsdReturnSwap" -> USDReturnSwap
        | "BtcReturnSwap" -> BTCReturnSwap
        | "LtcReturnSwap" -> LTCReturnSwap
        | _ -> InstError

  let string_of_symbol sym =
      match sym with
        | BtcUsd -> "btcusd"
        | LtcUsd -> "ltcusd"
        | LtcBtc -> "ltcbtc"
        | DrkUsd -> "drkusd"
        | DrkBtc -> "drkbtc"
        | Th1Btc -> "th1btc"
        | SymbolError -> "symbol error"

  let symbol_of_string s =
      match s with
        | "btcusd" -> BtcUsd
        | "ltcusd" -> LtcUsd
        | "ltcbtc" -> LtcBtc
        | "drkusd" -> DrkUsd
        | "drkbtc" -> DrkBtc
        | "th1btc" -> Th1Btc
        | _ -> SymbolError

  let wallet_kind_to_string w =
      match w with
        | Trading -> "trading"
        | Deposit -> "deposit"
        | Exchange -> "exchange"
        | WalletKindError -> "wallet error"

  let string_of_order_kind ok =
      match ok with
        | Buy -> "buy"
        | Sell -> "sell"
        | Short -> "sell"
        | Cover -> "buy"
        | OrderKindError -> "OrderKindError"

  let order_kind_of_string s =
      match s with
        | "buy" -> Buy
        | "sell" -> Sell
        | _ -> OrderKindError

  let string_of_order_type order =
      match order with
        | Limit -> "limit"
        | Market -> "market"
        | Stop -> "stop"
        | TrailingStop -> "trailing-stop"
        | FillOrKill -> "fill-or-kill"
        | ExchangeMarket -> "exchange market"
        | ExchangeLimit -> "exchange limit"
        | ExchangeStop -> "exchange stop"
        | ExchangeTrailingStop -> "exchange trailing-stop"
        | ExchangeFillOrKill -> "exchange fill-or-kill"
        | OrderTypeError -> "OrderTypeError"

  let order_type_of_string order =
      match order with
        | "limit" -> Limit
        | "market" -> Market
        | "stop" -> Stop
        | "trailing-stop" -> TrailingStop
        | "fill-or-kill" -> FillOrKill
        | "exchange market" -> ExchangeMarket
        | "exchange limit" -> ExchangeLimit
        | "exchange stop" -> ExchangeStop
        | "exchange trailing-stop" -> ExchangeTrailingStop
        | "exchange fill-or-kill" -> ExchangeFillOrKill
        | _ -> OrderTypeError

  (* Function to handle the is_hidden key of the JSON string *)
  let is_hidden (o : order) =
      if o.is_hidden = true
      then ("\"," ^ " \"is_hidden\": " ^ (string_of_bool o.is_hidden) ^ "}")
      else "\"}"

  (* Functions to get elements from JSON *)
  let get_int j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_int
  let get_symbol j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_string |> symbol_of_string
  let get_order_type j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_string |> 
                           order_type_of_string
  let get_order_kind j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_string |> 
                           order_kind_of_string
  let get_string j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_string
  let get_float j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_string |> float_of_string
  let get_bool j e = List.nth (to_assoc j) e |> fun (_,i) -> i |> to_bool
  let get_tm j e = List.nth (to_assoc j) e |> fun (_,t) -> t |> to_string |> float_of_string |> 
                   Unix.gmtime
  let get_inst j e = List.nth (to_assoc j) e |> fun (_,s) -> s |> to_string |> inst_of_string

  (* Get record for balance history from JSON *)
  let json_to_bal_hist json = {
    symbol = get_symbol json 0;
    amount = get_float json 1 ;
    balance = get_float json 2; 
    description = get_string json 3;
    timestamp = get_tm json 4
  }

  (* Receipt for new order *)
  (* having problems with get_string json 2 bc it is type `Null *)
  let json_to_order_receipt json = {
    order_id = get_int json 0;
    symbol = get_symbol json 1;
    exchange = "Bitfinex"(*get_string json 2*); 
    price = get_float json 3;
    avg_execution_price = get_float json 4;
    side = get_order_kind json 5; 
    order_type = get_order_type json 6;
    timestamp = get_tm json 7;
    is_live = get_bool json 8; 
    is_cancelled = get_bool json 9;
    is_hidden = get_bool json 10;
    was_forced = get_bool json 11;
    original_amount = get_float json 12;
    remaining_amount = get_float json 13;
    executed_amount = get_float json 14
  }

  (* Receipt for cancelled order *)
  let json_to_order_cancel_receipt json = {
    order_id = get_int json 0;
    symbol = get_symbol json 1;
    exchange = get_string json 2; 
    price = get_float json 3;
    avg_execution_price = get_float json 4;
    side = get_order_kind json 5; 
    order_type = get_order_type json 6;
    timestamp = get_tm json 7;
    is_live = get_bool json 8; 
    is_cancelled = get_bool json 9;
    is_hidden = get_bool json 10;
    was_forced = get_bool json 11;
    original_amount = get_float json 12;
    remaining_amount = get_float json 13;
    executed_amount = get_float json 14
  }

  let json_to_position json = {
    id = get_int json 0;
    symbol = get_symbol json 1;
    status = get_string json 2; 
    base = get_float json 3;
    amount = get_float json 4;
    timestamp = get_tm json 5; 
    swap = get_float json 6;
    pl = get_float json 7
  }

  let json_to_past_trade json = {
    price = get_float json 0;
    amount = get_float json 1;
    timestamp = get_tm json 2;
    exchange = get_string json 3;
    order_type = get_order_type json 4; 
    fee_currency = get_inst json 5;
    fee_amount = get_float json 6;
    tid = get_int json 7; 
    order_id = get_int json 8
  }

  let string_of_inst inst = 
    match inst with
    | USD -> "usd"
    | BTC -> "btc" 
    | LTC -> "ltc"  
    | USDReturnSwap -> "usdreturnswap"
    | BTCReturnSwap -> "btcreturnswap"
    | LTCReturnSwap -> "ltcreturnswap"
    | InstError -> "Error: Invalid inst type passed to string_of_inst"

  (* SHA384 *)
  let to_sha384 msg =
    let string_of_hex h =
      match h with
      | `Hex h -> h
      | _ -> ""
    in
    let my_key = Nocrypto.Uncommon.Cs.of_hex (string_of_hex @@ Hex.of_string private_key) in
    let my_data = Nocrypto.Uncommon.Cs.of_hex (string_of_hex @@ Hex.of_string msg) in
    Nocrypto.Hash.mac `SHA384 ~key:my_key my_data
    |> Cstruct.to_string |> Hex.of_string |> string_of_hex

  (* base64 encode *)
  let encode_base64 msg = Cstruct.of_string msg |> Nocrypto.Base64.encode |> Cstruct.to_string

  (* Get ticker *)
  let get_ticker sym = 
    lwt json_response = get_it ("pubticker/" ^ (string_of_symbol sym)) in
    let f x = member x json_response |> to_string |> float_of_string in
    Lwt.return {
      mid = f "mid";
      bid = f "bid";
      ask = f "ask";
      last_price = f "last_price";
      low = f "low";
      high = f "high";
      volume = f "volume";
      ticker_time = f "timestamp"
    }
    
  (* Get lendbook  - returns [bids : lendbook list; asks : lendbook list] *)
  let get_lendbook ccy = 
    lwt json_response = get_it ("lendbook/" ^ ccy) in
    let bids = json_response |> member "bids" |> to_list in
    let asks = json_response |> member "asks" |> to_list in
    let bid_rates =
      bids |> List.map (fun json_assoc -> json_assoc |> member "rate" |>
      to_string |> float_of_string)
    in
    let bid_amounts =
      bids |> List.map (fun json_assoc -> json_assoc |> member "amount" |>
      to_string |> float_of_string)
    in
    let bid_periods =
      bids |> List.map (fun json_assoc -> json_assoc |> member "period" |> to_int)
    in
    let bid_times =
      bids |> List.map
      (fun json_assoc ->
        json_assoc |> member "timestamp" |> to_string |> float_of_string |> Unix.gmtime)
    in
    let bid_frrs =
      bids |> List.map (fun json_assoc -> json_assoc |> member "frr" |> to_string) |>
      List.map (fun frr -> match frr with "Yes" -> true | "No" -> false | _ -> false)
    in
    let ask_rates =
      asks |> List.map
      (fun json_assoc -> json_assoc |> member "rate" |> to_string |> float_of_string)
    in
    let ask_amounts =
      asks |> List.map
      (fun json_assoc -> json_assoc |> member "amount" |> to_string |> float_of_string)
    in
    let ask_periods =
      asks |> List.map (fun json_assoc -> json_assoc |> member "period" |> to_int)
    in
    let ask_times = 
      asks |> List.map
      (fun json_assoc ->
        json_assoc |> member "timestamp" |> to_string |> float_of_string |> Unix.gmtime)
    in
    let ask_frrs = 
      asks |>
      List.map (fun json_assoc -> json_assoc |> member "frr" |> to_string) |>
      List.map (fun frr -> match frr with "Yes" -> true | "No" -> false | _ -> false)
    in
    let rec combine_lists l1 l2 l3 l4 l5 (init : lendbook list) = 
      let f = List.hd in
      let g = List.tl in
      match l1 with
      | [] -> init 
      | hd :: tl -> combine_lists (g l1) (g l2) (g l3) (g l4) (g l5) 
                                  ({rate = f l1; amount = f l2; period = f l3; 
                                    timestamp = f l4; frr = f l5} :: init)
    in
    Lwt.return
    [combine_lists bid_rates bid_amounts bid_periods bid_times bid_frrs [];
     combine_lists ask_rates ask_amounts ask_periods ask_times ask_frrs []]

  (* Get orderbook  - returns [bids : orderbook list; asks : orderbook list] *)
  let get_orderbook (sym : symbol) =
    lwt json_response = get_it ("book/" ^ string_of_symbol sym) in
    let bids = json_response |> member "bids" |> to_list in
    let asks = json_response |> member "asks" |> to_list in
    let bid_prices = 
      bids |> List.map
      (fun json_assoc -> json_assoc |> member "price" |> to_string |> float_of_string)
    in
    let bid_amounts =
      bids |> List.map
      (fun json_assoc -> json_assoc |> member "amount" |> to_string |> float_of_string)
    in
    let bid_times =
      bids |> List.map
      (fun json_assoc ->
        json_assoc |> member "timestamp" |> to_string |> float_of_string |> Unix.gmtime)
    in
    let ask_prices =
      asks |> List.map (fun json_assoc -> json_assoc |> member "price" |> to_string |> float_of_string)
    in
    let ask_amounts =
      asks |> List.map
      (fun json_assoc -> json_assoc |> member "amount" |> to_string |> float_of_string)
    in
    let ask_times =
      asks |> List.map
      (fun json_assoc ->
        json_assoc |> member "timestamp" |> to_string |> float_of_string |> Unix.gmtime)
    in
    let rec combine_lists l1 l2 l3 (init : orderbook list) = 
      let f = List.hd in
      let g = List.tl in
      match l1 with
      | [] -> init 
      | hd :: tl -> combine_lists (g l1) (g l2) (g l3)
                                  ({price = f l1; amount = f l2; timestamp = f l3} :: init)
    in
    Lwt.return
    [combine_lists bid_prices bid_amounts bid_times [];
     combine_lists ask_prices ask_amounts ask_times []]

  (* Get trades - get all trades on or after (t : float) *)
  let get_trades ?(limit_trades=1000) t =
    lwt json_response =
      get_it ("trades/btcusd/?timestamp=" ^ (string_of_float t) ^
              "/?limit_trades=" ^ (string_of_int limit_trades))
      >>= fun json -> json |> to_list |> Lwt.return
    in
    let prices =
      json_response |> filter_member "price" |> List.map (fun x -> to_string x |> float_of_string)
    in
    let amounts =
      json_response |> filter_member "amount" |> List.map (fun x -> to_string x |> float_of_string)
    in
    let times =
      json_response |> filter_member "timestamp"
      |> List.map (fun x -> to_int x |> float_of_int |> Unix.gmtime)
    in
    let rec combine_lists l1 l2 l3 (init : trade list) = 
      let f = List.hd in
      let g = List.tl in
      match l1 with
      | [] -> init 
      | hd :: tl -> combine_lists (g l1) (g l2) (g l3)
                                  ({price = f l1; amount = f l2; timestamp = f l3} :: init)
    in
    Lwt.return @@ combine_lists prices amounts times []

  (* Get a list of the available symbols *)
  let get_symbols () =
    lwt json_response = get_it "symbols" in
    json_response |> to_list |> List.map (fun json -> to_string json |> symbol_of_string)
    |> Lwt.return

  let get_wallet_balance () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
        " \"request\": \"/v1/balances\"," ^
        " \"options\": {}}" 
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    lwt json_response =
      post_it "balances" base64_payload encrypted_signature
      >>= fun json -> to_list json |> Lwt.return
    in
    let get_type s = 
      match s with 
      | "trading" -> Trading
      | "deposit" -> Deposit
      | "exchange" -> Exchange
      | _ -> WalletKindError
    in
    let get_insts s = 
      match s with 
      | "usd" -> USD
      | "btc" -> BTC
      | "ltc" -> LTC
      (* TODO: check that the rest of these matching strings are correct *)
      | "usdreturnswap" -> USDReturnSwap
      | "btcreturnswap" -> BTCReturnSwap
      | "ltcreturnswap" -> LTCReturnSwap
      | _ -> InstError
    in
    let types = 
      json_response |> filter_member "type"
      |> List.map (fun json_s -> to_string json_s |> get_type)
    in
    let insts =
      json_response |> filter_member "currency"
      |> List.map (fun json_s -> to_string json_s |> get_insts)
    in
    let amounts =
      json_response |> filter_member "amount"
      |> List.map (fun json_s -> to_string json_s |> float_of_string)
    in
    let available =
      json_response |> filter_member "available"
      |> List.map (fun json_s -> to_string json_s |> float_of_string)
    in
    let rec combine_lists l1 l2 l3 l4 (init : wallet list) = 
      let f = List.hd in
      let g = List.tl in
      match l1 with
      | [] -> init 
      | hd :: tl -> combine_lists (g l1) (g l2) (g l3) (g l4)
                                  ({wallet_kind = f l1; instrument = f l2; amount = f l3; 
                                    available = f l4} :: init)
    in
    Lwt.return @@ combine_lists types insts amounts available []

  (* Send a new order to the Bitfinex exchange *)
  (* NOTE: Order price must always be positive *)
  let new_order (odr : order) = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/new\"," ^
      " \"symbol\": \"" ^ (string_of_symbol odr.symbol) ^ "\"," ^
      " \"amount\": \"" ^ (string_of_float odr.amount) ^ "0" ^ "\"," ^
      " \"price\": \"" ^ (string_of_float odr.price) ^ "0" ^ "\"," ^
      " \"exchange\": \"bitfinex\"," ^
      " \"side\": \"" ^ (string_of_order_kind odr.order_kind) ^ "\"," ^
      " \"type\": \"" ^ (string_of_order_type odr.order_type) ^ 
      (is_hidden odr)
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (post_it "order/new" base64_payload encrypted_signature
       >>= fun json -> Success (json_to_order_receipt json) |> Lwt.return)
    with
      _ -> Lwt.return Failure

  (* Multiple New Orders *)
  (* NOTE/TODO: Cannot place more than ten orders at once. Add check for this.*)
  let new_multi_order (odrs : order list) = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let order_to_json (odr : order) = 
      "{\"symbol\": \"" ^ (string_of_symbol odr.symbol) ^ "\"," ^
      " \"amount\": \"" ^ (string_of_float odr.amount) ^ "0" ^ "\"," ^
      " \"price\": \"" ^ (string_of_float odr.price) ^ "0" ^ "\"," ^
      " \"exchange\": \"bitfinex\"," ^
      " \"side\": \"" ^ (string_of_order_kind odr.order_kind) ^ "\"," ^
      " \"type\": \"" ^ (string_of_order_type odr.order_type) ^
      (is_hidden odr)
    in
    let orders_list = List.map order_to_json odrs in
    let rec to_json json l = 
      match (json, l) with
      | (_, []) -> "[" ^ json ^ "]"
      | ("", hd :: tl) -> to_json hd tl
      | (_, hd :: tl) -> to_json (json ^ "," ^ hd) tl
    in
    let multi_orders_json = to_json "" orders_list in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/new/multi\"," ^
      " \"orders\": " ^ multi_orders_json ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    lwt result =
      post_it "order/new/multi" base64_payload encrypted_signature 
      >>= fun s -> to_assoc s |> List.tl |> List.hd |> fun (_,s) -> s |> to_string |> Lwt.return
    in
    match result with
    | "success" -> Lwt.return MultiOrderSuccess
    | _ -> Lwt.return Failure

  let cancel_order order_id = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/cancel\"," ^
      " \"order_id\": " ^ (string_of_int order_id) ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (post_it "order/cancel" base64_payload encrypted_signature 
       >>= fun json -> Success (json_to_order_cancel_receipt json) |> Lwt.return)
    with
      _ -> Lwt.return Failure

  (* Cancel Multiple Orders *)
  let cancel_multi_order order_ids = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    (*let order_id_to_json order_id = "{" ^ (string_of_int order_id) ^ "}" in*)
    let rec to_json json l = 
      match (json, l) with
      | ("", hd :: tl) -> to_json hd tl
      | (_, hd :: tl) -> to_json (json ^ "," ^ hd) tl
      | (_, []) -> "[" ^ json ^ "]"
    in
    let orders_list = List.map string_of_int order_ids in
    let multi_orders_json = to_json "" orders_list in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/cancel/multi\"," ^
      " \"order_ids\": " ^ multi_orders_json ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (post_it "order/cancel/multi" base64_payload encrypted_signature 
       >>= fun json -> Success (json_to_order_cancel_receipt json) |> Lwt.return)
    with
      _ -> Lwt.return Failure

  let cancel_all_orders () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/cancel/all\"," ^
      " \"options\": {}}" 
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (post_it "order/cancel/all" base64_payload encrypted_signature
       >>= fun json -> to_string json |> Lwt.return)
    with
      _ -> Lwt.return "Failed to cancel all orders"

  (* Replace Order *)
  (* odr - order_receipt of order to be replaced, new_price - new price of order *)
  let replace_order_price (odr : order_receipt) new_price = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/cancel/replace\"," ^
      " \"order_id\": " ^ (string_of_int odr.order_id) ^ "," ^
      " \"symbol\": \"" ^ (string_of_symbol odr.symbol) ^ "\"," ^
      " \"amount\": \"" ^ (string_of_float odr.remaining_amount) ^ "0" ^ "\"," ^
      " \"price\": \"" ^ (string_of_float new_price) ^ "0" ^ "\"," ^
      " \"exchange\": \"bitfinex\"," ^
      " \"side\": \"" ^ (string_of_order_kind odr.side) ^ "\"," ^
      " \"type\": \"" ^ (string_of_order_type odr.order_type) ^ "\"}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (post_it "order/cancel/replace" base64_payload encrypted_signature 
       >>= fun json -> Success (json_to_order_receipt json) |> Lwt.return)
    with
      _ -> Lwt.return Failure

  (* Order Status *)
  (* Returns true if order is active, false if cancelled or order does not exist *)
  let order_status order_id = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/order/status\"," ^
      " \"order_id\": " ^ (string_of_int order_id) ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try
      (post_it "order/status" base64_payload encrypted_signature 
       >>= fun json -> member "is_live" json |> to_bool |> Lwt.return)
    with
      _ -> Lwt.return false

  (* Active Orders *)
  let active_orders () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/orders\"," ^
      " \"options\": {}}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    try_lwt
      (
        post_it "orders" base64_payload encrypted_signature
         >>= fun json -> to_list json
                         |> List.map (fun assoc -> Success (json_to_order_receipt assoc))
                         |> Lwt.return
      )
    with
      _ -> Lwt.return [Failure]

  (* Active Order Ids - get a list of the active order IDs*)
  (* errors = true = return errors as -1, false = remove errors *)
  let active_order_ids ?(errors=false) sym = 
    let filter_on_sym (res : order_result) =
      match res with
      | Success r -> r.symbol = sym
      | _ -> false
    in
    let get_order_result (res : order_result) = 
      match res with
      | Success r -> r.order_id
      | _ -> -1 (* error *)
    in
    let filter_errors l = if errors = true then l else List.filter (fun i -> i != (-1)) l in
    active_orders () 
    >>= fun orl -> List.filter filter_on_sym orl |> List.map get_order_result |> filter_errors
                   |> Lwt.return

  (* TODO: Fix this so that it does something better than returning [] when under an exception *)
  let active_positions symbol = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/positions\"," ^
      " \"options\": {}}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    post_it "positions" base64_payload encrypted_signature
    >>= fun json ->
      try_lwt
        to_list json |> List.map json_to_position 
        |> List.filter (fun (r : position) -> (r.symbol = symbol) && (r.status = "ACTIVE"))
        |> Lwt.return
      with
      | _ -> Lwt.return []

  (* Claim position *) (* TODO: FINISH THIS *)
  let claim_position pos_id = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/position/claim\"," ^
      " \"position_id\": " ^ (string_of_int pos_id) ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    let open Yojson.Basic.Util in
    post_it "position/claim" base64_payload encrypted_signature

  (* Balance History Full - returns all info *)
  let balance_history_full ?(t_start=0) ?(t_end=0) ?(limit=500) (i : inst) (w : wallet_kind)= 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let t_since = if t_start > 0 then " \"since\": " ^ (string_of_int t_start) ^ "," else "" in
    let t_end = if t_end > 0 then " \"until\": " ^ (string_of_int t_end) ^ "," else "" in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/history\"," ^
      " \"currency\": \"" ^ (string_of_inst i) ^ "\"," ^ t_since ^ t_end ^
      " \"limit\": " ^ (string_of_int limit) ^ "," ^
      " \"wallet\": \"" ^ (wallet_kind_to_string w) ^ "\"" ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    post_it "history" base64_payload encrypted_signature
    >>= fun json -> to_list json |> List.map json_to_bal_hist |> Lwt.return

  (* Balance History - returns the balances only *)
  let balance_history ?(t_start=0) ?(t_end=0) ?(limit=500) (i : inst) (w : wallet_kind)= 
    balance_history_full ~t_start:t_start ~t_end:t_end ~limit:limit i w
    >>= fun json -> List.map (fun r -> r.balance) json |> Lwt.return

  (* Past Trades - omit_before defaults to one day ago *)
  let past_trades ?(limit=50)
                  ?(omit_before = (int_of_float @@ Unix.time ()) - 43200)
                   (sym : symbol) = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/mytrades\"," ^
      " \"symbol\": \"" ^ (string_of_symbol sym) ^ "\"," ^
      " \"timestamp\": " ^ (string_of_int omit_before) ^ "," ^
      " \"limit_trades\": " ^ (string_of_int limit) ^ "}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    post_it "mytrades" base64_payload encrypted_signature
    >>= fun json -> to_list json |> List.map json_to_past_trade |> Lwt.return

  (* TODO: Functions below this point still need work *)

  (* still needs work *)
  let taken_swaps () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/taken_swaps\"," ^
      " \"options\": {}}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    let open Yojson.Basic.Util in
    post_it "taken_swaps" base64_payload encrypted_signature

  (* still needs work *)
  let account_information () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/account_infos\"," ^
      " \"options\": {}}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    let open Yojson.Basic.Util in
    post_it "account_infos" base64_payload encrypted_signature

  (* still needs work *)
  let margin_information () = 
    let nonce = string_of_int @@ (int_of_float @@ Unix.time ()) * 100000 in
    let payload = 
      "{\"nonce\": \"" ^ nonce ^ "\"," ^
      " \"request\": \"/v1/margin_infos\"," ^
      " \"options\": {}}"
    in
    let base64_payload = encode_base64 payload in
    let encrypted_signature = to_sha384 base64_payload in
    let open Yojson.Basic.Util in
    post_it "margin_infos" base64_payload encrypted_signature
    (*|> to_list |> List.map json_to_position |> List.map (fun pos -> pos.id)*)

end
