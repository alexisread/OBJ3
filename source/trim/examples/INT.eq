--- Module: INT
--- psort Int
--- sorts Int Nat 
subsorts Nat < Int
op _+_ : Int Int -> Int
op s : Int -> Int
op s : Nat -> Nat
op 0 : -> Nat
{

--- eq M + 0 = M
eq (op 2 _+_ Int (var M [ Nat ])(op 0 0 Nat)) => (var M [ Nat ]) .

--- eq M + s(N) = s(M + N)
eq (op 2 _+_ Int (var M [ Nat ])(op 1 s Nat (var N [ Nat ]))) => (op 1 s Int 
    (op 2 _+_ Int (var M [ Nat ])(var N [ Nat ]))) .

}
