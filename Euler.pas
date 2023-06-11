Unit Euler;


Interface
uses math,sysutils,System.StrUtils,Velthuis.BigIntegers,System.Generics.Collections;

CONST
  to_be_checked = '123456789';
    dividers: ARRAY [2 .. 6] OF uint32 = (10, 100, 1000, 10000, 100000);

  function Sieve(Limit: Integer): TArray<Boolean>;
  Function IsPrime(Number: UInt64): Boolean;
  function IsPalindrome(number: Integer): Boolean; overload
  function IsPalindrome(number: uint64): Boolean; overload
  FUNCTION IsPalindrome(const str: STRING): boolean; OVERLOAD;
  function IsPalindrome(const str: biginteger): Boolean; overload
  Function NumberOfDivisors(lin: integer): integer;
  Function GetSumOfDigits(num: uint64): integer;overload;
  Function GetSumOfDigits(num: STRING): integer;overload;
  Function GetSumOfDigits(numbg: biginteger): integer;overload;
  Function sum_of_divisors(lin: integer): integer;
  FUNCTION intpower(base, expon: longint): longint;
  FUNCTION GetDigit(num, digit: uint64): integer;
  FUNCTION Num_Of_Digits(i: uint64): longint;  overload
  FUNCTION Num_Of_Digits(i: integer): longint; overload
  FUNCTION sortstring(woord: STRING): STRING;

  FUNCTION sort_number(x : uint64) : uint64;
  FUNCTION IsPandigital(s: STRING): boolean;
  function IntToBin(number: integer): string;
  function BinarySearch(const arr: array of Integer; const x: Integer): Boolean;

  FUNCTION IsPentagonal(num: uint64): boolean;
  FUNCTION IsHexagonal(num: uint64): boolean;

  function HasRepeatedDigit(number: Uint32): integer;
  FUNCTION Reverse(num: BigInteger): BigInteger;
  function IsLychrelNumber(num: biginteger): Boolean;
  function Square(n : uint64) : uint64;
  function Triangle(n : uint64) : uint64;
  function Pentagonal(n : uint64) : uint64;
  function Hexagonal(n : uint64) : uint64;
  function Heptagonal(n : uint64) : uint64;
  function Octagonal(n : uint64) : uint64;
  function gcd(a, b: UInt64): UInt64;
  function RevSortString(woord: string): string;


Implementation
Function Sieve(Limit: Integer): TArray<Boolean>;
Var
  I, J, K: Integer;
Begin
  SetLength(Result, Limit + 1);
  Result[2] := True;

  For I := 3 To Limit Do
    Result[I] := (I Mod 2 = 1);

  For I := 3 To Trunc(Sqrt(Limit)) Do
  Begin
    If Result[I] Then
    Begin
      J := I * I;
      K := I Shl 1;

      While J <= Limit Do
      Begin
        Result[J] := False;
        Inc(J, K);
      End;
    End;
  End;

  For I := 4 To Limit Do
  Begin
    If Result[I] And (I Mod 2 = 0) Then
      Result[I] := False;
  End;
End;

Function IsPrime(Number: UInt64): Boolean;
Const
  Primes: Array [0 .. 7] Of UInt64 = (2, 3, 5, 7, 11, 13, 17, 19);
Var
  I: Uint32;
Begin
  If (Number <= 1) Or ((Number > 2) And ((Number And 1) = 0)) Then
  Begin
    Result := False;
    Exit;
  End;
  For I := 0 To High(Primes) Do
  Begin
    If Primes[I] * Primes[I] > Number Then
      Break;
    If (Number Mod Primes[I] = 0) Then
    Begin
      Result := False;
      Exit;
    End;
  End;
  I := Primes[High(Primes)];
  While (I * I) <= Number Do
  Begin
    If (Number Mod I = 0) Then
    Begin
      Result := False;
      Exit;
    End;
    Inc(I, 2);
  End;
  Result := True;
End;

function IsPalindrome(number: Integer): Boolean; overload
var
  originalNumber, reversedNumber, digit: Integer;
begin
  originalNumber := number;
  reversedNumber := 0;
  while number > 0 do
  begin
    digit := number mod 10;
    reversedNumber := (reversedNumber * 10) + digit;
    number := number div 10;
  end;
  Result := originalNumber = reversedNumber;
end;

function IsPalindrome(number: uint64): Boolean; overload
var
  originalNumber, reversedNumber, digit: uint64;
begin
  originalNumber := number;
  reversedNumber := 0;
  while number > 0 do
  begin
    digit := number mod 10;
    reversedNumber := (reversedNumber * 10) + digit;
    number := number div 10;
  end;
  Result := originalNumber = reversedNumber;
end;

function IsPalindrome(const str: string): Boolean;
begin
  result := (str = ReverseString(str));
end;

function IsPalindrome(const str: biginteger): Boolean; overload
begin
  result := (str.ToString = ReverseString(str.ToString));
end;

Function NumberOfDivisors(lin: integer): integer;

VAR
  lcnt, nod: integer;
BEGIN
  lcnt := 1;
  nod := 0;
  WHILE lcnt * lcnt <= lin DO
    BEGIN
      IF lin MOD lcnt = 0 THEN
        Inc(nod, 2);
      inc(lcnt);
    END;
  dec(lcnt);
  IF lcnt * lcnt = lin THEN
    dec(nod);
  NumberOfDivisors := nod;
END;

Function GetSumOfDigits(num: uint64): integer;overload;
// Returns sums of all digits

VAR
  sum  : integer;
  dummy: uint64;
BEGIN
  sum := 0;
  REPEAT
    dummy := num;
    num := num DIV 10;
    Inc(sum, dummy - (num SHL 3 + num SHL 1));
  UNTIL num < 1;
  Result := sum;
END;

Function GetSumOfDigits(num: STRING): integer;
overload;

VAR i,sm : integer;
BEGIN
  sm := 0;
  FOR i := 1 TO length(num) DO
    sm := sm + ord(num[i])-48;
  result := sm;
END;

Function GetSumOfDigits(numbg: biginteger): integer;
overload;

VAR i,sm : integer;
    num : string;
BEGIN
  num := numbg.ToString;
  sm := 0;
  FOR i := 1 TO length(num) DO
    sm := sm + ord(num[i])-48;
  result := sm;
END;

Function sum_of_divisors(lin: integer): integer;

VAR
  lcnt, sod: integer;
BEGIN
  sod := 1;
  lcnt := 2;
  WHILE lcnt * lcnt <= lin DO
    BEGIN
      IF lin MOD lcnt = 0 THEN
        BEGIN
          sod := sod + lcnt + lin DIV lcnt;
          IF lcnt * lcnt = lin THEN
            dec(sod, lcnt);
        END;
      lcnt := lcnt + 1;
    END;
  sum_of_divisors := sod;
END;

FUNCTION intpower(base, expon: longint): longint;

VAR
  i, ip: longint;
BEGIN
  ip := 1;
  FOR i := 1 TO expon DO
    ip := ip * base;
  intpower := ip;
END;

FUNCTION GetDigit(num, digit: uint64): integer;
// returns value of a given digit counting from the right (ones first)

VAR
  z: integer;

BEGIN
  FOR z := 1 TO digit - 1 DO
    num := num DIV 10;
  Result := num MOD 10;
END;

FUNCTION Num_Of_Digits(i: uint64): integer; overload
BEGIN
  Result := Floor(Log10(i)) + 1;
END;

FUNCTION Num_Of_Digits(i: integer): integer; overload
BEGIN
  Result := Floor(Log10(i)) + 1;
END;

function SortString(woord: string): string;
var
  flip: Boolean;
  stop, i: Integer;
  dummy: Char;
begin
  stop := Length(woord);
  repeat
    flip := False;
    Dec(stop);
    for i := 1 to stop do
    begin
      if woord[i] > woord[i + 1] then
      begin
        dummy := woord[i + 1];
        woord[i + 1] := woord[i];
        woord[i] := dummy;
        flip := True;
      end;
    end;
  until not flip;
  Result := woord;
end;

FUNCTION sort_number(x : uint64) : uint64;
BEGIN
  result := strtoint(sortstring(inttostr(x)));
END;

FUNCTION IsPandigital(s: STRING): boolean;
BEGIN
  Result := (length(s) = 9) AND (sortstring(s) = to_be_checked);
END;

function IntToBin(number: integer): string;
var
  i: Integer;
  start : Boolean;
begin
  Result := '';
  start := false;
  for i := 31 downto 0 do
  begin
    if (number and (1 shl i)) > 0 then
      begin
        Result := Result + '1';
        start := true;
      end
    else
      if start then
        Result := Result + '0';
  end;
end;

function BinarySearch(const arr: array of Integer; const x: Integer): Boolean;
var
  L, R, mid: Integer;
begin
  Result := False;
  L := 0;
  R := High(arr);

  while L <= R do
  begin
    mid := (L + R) div 2;
    if arr[mid] = x then
    begin
      Result := True;
      Exit;
    end
    else if arr[mid] < x then
      L := mid + 1
    else
      R := mid - 1;
  end;
end;

function IsPentagonal(num: uint64): Boolean;
var
  n: uint64;
begin
  n := Trunc((1 + Sqrt(1 + 24 * num)) / 6);
  Result := (n * (3 * n - 1) = 2 * num);
end;

function IsHexagonal(num: uint64): Boolean;
var
  n: uint64;
begin
  n := Trunc((1 + Sqrt(1 + 8 * num)) / 4);
  Result := (n * (2 * n - 1) = num);
end;

function HasRepeatedDigit(number: UInt32): Integer;
var
  digitCounts: array[0..9] of uint32;
  digit,i: UInt32;

begin
  number := number div 10; // least significant digit cannot be repeating
  FillChar(digitCounts, SizeOf(digitCounts), 0);

  while (number > 0) do
  begin
    digit := number mod 10;
    Inc(digitCounts[digit]);
    number := number div 10;
  end;

  for i := 0 to 2 do
  begin
    if digitCounts[i] >= 3 then
      Exit(i);
  end;

  Result := -1;
end;

FUNCTION Reverse(num: BigInteger): BigInteger;
BEGIN
  result := 0;
  WHILE (num > 0) DO
    BEGIN
      result := result * 10 + (num MOD 10);
      num := num DIV 10;
    END;
END;

function IsLychrelNumber(num: biginteger): Boolean;
const
  MaxIterations = 50; // Maximum number of iterations to check
var
  iteration: Integer;
  reversed: biginteger;
begin
  iteration := 1;
  reversed := Reverse(num);
   while (iteration <= MaxIterations) and (not IsPalindrome(num + reversed)) do
  begin
    num := num + reversed;
    reversed := Reverse(num);
    Inc(iteration);
  end;
  Result := iteration > MaxIterations;
end;

function Square(n : uint64) : uint64;
begin
  result := n * n;
end;

function Triangle(n : uint64) : uint64;
begin
  result := trunc(n*(n+1)/2);
end;

function Pentagonal(n : uint64) : uint64;
begin
  result := trunc(n*(3*n-1)/2);
end;

function Hexagonal(n : uint64) : uint64;
begin
  result := n*(2*n-1);
end;

function Heptagonal(n : uint64) : uint64;
begin
  result := trunc(n*(5*n-3)/2);
end;

function Octagonal(n : uint64) : uint64;
begin
  result := n*(3*n-2);
end;

function gcd(a, b: UInt64): UInt64;
begin
  while b <> 0 do
  begin
    a := a mod b;
    a := a xor b;
    b := b xor a;
    a := a xor b;
  end;
  result := a;
end;

function RevSortString(woord: string): string;
begin
  Result := ReverseString(sortstring(woord));
end;



End.
