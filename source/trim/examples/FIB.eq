--- Module: FIB
--- protecting MYNAT
--- psort Nat
--- sort Nat 
op 0 : -> Nat
op s : Nat -> Nat
op _+_ : Nat Nat -> Nat
op fib : Nat -> Nat
{

--- eq M + s(N) = s(M + N)
eq (op 2 _+_ Nat (var M [ Nat ])(op 1 s Nat (var N [ Nat ]))) => (op 1 s Nat 
    (op 2 _+_ Nat (var M [ Nat ])(var N [ Nat ]))) .

--- eq M + 0 = M
eq (op 2 _+_ Nat (var M [ Nat ])(op 0 0 Nat)) => (var M [ Nat ]) .

--- eq fib(0) = 0
eq (op 1 fib Nat (op 0 0 Nat)) => (op 0 0 Nat) .

--- eq fib(s(0)) = s(0)
eq (op 1 fib Nat (op 1 s Nat (op 0 0 Nat))) => (op 1 s Nat (op 0 0 Nat)) .

--- eq fib(s(s(N))) = fib(N) + fib(s(N))
eq (op 1 fib Nat (op 1 s Nat (op 1 s Nat (var N [ Nat ])))) => (op 2 _+_ Nat 
    (op 1 fib Nat (var N [ Nat ]))(op 1 fib Nat (op 1 s Nat (var N [ Nat ])))) .

}
