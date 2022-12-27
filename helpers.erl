%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-1

-module(helpers).
-import(string, [concat/2 , substr/3, to_lower/1, right/3]).
-import(base64, [encode_to_string/1]).
-import(lists,[duplicate/2]).
-export([gen_rand_str/1, hash_str/1, get_leading_zeros/2]).

% Generate random string
gen_rand_str(L) ->
  % Args:
  % S: Length of the string

  Rand_Str = substr(encode_to_string(crypto:strong_rand_bytes(32)), 1, L),
  concat("kumbam.pranath;", Rand_Str).

% Generate SHA256 hash
hash_str(S) ->
  % Args:
  % S: String to be hashed

  <<X:256, _/binary>> = crypto:hash(sha256, S),
  Hash = to_lower(right(integer_to_list(X,16), 64, $0)),
  Hash.

% Return leading digits of hash and a substring of zeroes
get_leading_zeros(H, K) ->
  % Args:
  % H: Hash
  % K: No of leading zeroes required

  Lst1 = duplicate(K,"0"),
  Z = binary_to_list(list_to_binary(Lst1)),
  H2 = substr(H, 1, K),
  [Z | H2].
