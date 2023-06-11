unit PrimeUtils;

interface

uses
  System.Math, System.Types;

type
  PrimesU = class
  private
    class function ModPow(base: UInt64; exponent: UInt64; modulus: UInt64): UInt64;
    class function Witness(a: UInt64; n: UInt64): Boolean;
    class function CompositeWitness(n: UInt64; d: UInt64; s: UInt32; a: UInt64): Boolean;
  public
    class function Sieve(maxValue: UInt32): TArray<Boolean>;
    class function IsPrime(n: UInt64): Boolean;
  end;

implementation

class function PrimesU.ModPow(base: UInt64; exponent: UInt64; modulus: UInt64): UInt64;
var
  curPower: UInt64;
begin
  result := 1;
  curPower := base mod modulus;
  while exponent > 0 do
  begin
    if exponent and 1 = 1 then
      result := (result * curPower) mod modulus;
    curPower := (curPower * curPower) mod modulus;
    exponent := exponent shr 1;
  end;
end;

class function PrimesU.Witness(a: UInt64; n: UInt64): Boolean;
var
  d, s: UInt64;
begin
  d := n - 1;
  s := 0;
  while (d and 1) = 0 do
  begin
    d := d shr 1;
    Inc(s);
  end;
  Result := CompositeWitness(n, d, s, a);
end;

class function PrimesU.CompositeWitness(n: UInt64; d: UInt64; s: UInt32; a: UInt64): Boolean;
var
  x, y: UInt64;
  i: UInt32;
begin
  x := ModPow(a, d, n);
  if (x = 1) or (x = n - 1) then
    Exit(True);
  for i := 1 to s - 1 do
  begin
    y := (x * x) mod n;
    if y = 1 then
      Exit(False);
    if y = n - 1 then
      Exit(True);
    x := y;
  end;
  Result := False;
end;

class function PrimesU.Sieve(maxValue: UInt32): TArray<Boolean>;
var
  primeFlags: TArray<Boolean>;
  i, j: UInt32;
begin
  SetLength(primeFlags, maxValue + 1);
  primeFlags[0] := False;
  primeFlags[1] := False;
  for i := 2 to maxValue do
    primeFlags[i] := True;
  for i := 2 to Trunc(Sqrt(maxValue)) do
  begin
    if primeFlags[i] then
    begin
      j := i * i;
      while j <= maxValue do
      begin
        primeFlags[j] := False;
        Inc(j, i);
      end;
    end;
  end;
  Result := primeFlags;
end;


class function PrimesU.IsPrime(n: UInt64): Boolean;
const
  // Miller-Rabin constants
  WitnessCount = 10;
  WitnessList: array[0..9] of UInt64 =
    (2, 325, 9375, 28178, 450775, 9780504, 1795265022, $1FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF);
var
  d: UInt64;
  s: UInt32;
  i: Integer;
  a: UInt64;
begin
  // Handle small numbers
  if n < 2 then
    Exit(False);
  if n <= 3 then
    Exit(True);
  // Write n-1 as 2^s * d, where d is odd
  d := n - 1;
  s := 0;
  while (d and 1) = 0 do
  begin
    d := d shr 1;
    Inc(s);
  end;
  // Use Miller-Rabin primality test
  for i := 0 to WitnessCount - 1 do
  begin
    a := WitnessList[i];
    if (a >= n) then
      Continue;
    if (not CompositeWitness(n, d, s, a)) then
      Exit(False);
  end;
  Result := True;
end;
end.

