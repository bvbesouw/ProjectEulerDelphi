UNIT problem021to030;

INTERFACE

USES Classes, Sysutils, System.Generics.Collections, Euler,
  Velthuis.BigIntegers;
FUNCTION Problem021: Integer;
FUNCTION Problem022: Integer;
FUNCTION Problem023: Integer;
FUNCTION Problem024: STRING;
FUNCTION Problem025: Integer;
FUNCTION Problem026: Integer;
FUNCTION Problem027: Integer;
FUNCTION Problem028: Uint64;
FUNCTION Problem029: Integer;
FUNCTION Problem030: Integer;

IMPLEMENTATION

FUNCTION Problem021: Integer;
VAR A, B: Integer;

BEGIN
  Result := 0;
  FOR A := 3 TO 10000 DO BEGIN
    B := Sum_of_divisors(A);
    IF (A <> B) AND (Sum_of_divisors(B) = A) THEN Inc(Result, A);
  END;
END;

FUNCTION Problem022: Integer;
CONST C_FNAME =
    'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\problem_22.txt';

VAR Names: TStringList; X, Y, Sm: Integer;

BEGIN
  Names := TStringList.Create;

  TRY Names.LoadFromFile(C_FNAME);
  EXCEPT
    ON E: EInOutError DO
        Writeln('File handling error occured. Reason: ', E.Message);

  END;

  Names.Delimiter := Char(',');
  Names.DelimitedText := Names.Text;
  Names.Sort;
  Result := 0;
  FOR X := 0 TO Pred(Names.Count) DO BEGIN
    Sm := 0;
    FOR Y := 1 TO Length(Names[X]) DO Inc(Sm, Ord(Names[X][Y]) - 64);
    Inc(Result, Sm * (X + 1));
  END;
  Names.Free;
END;

FUNCTION Problem023: Integer;
CONST MAX_NUM = 28123;
VAR SumOfDivisors: TArray<Integer>; AbundantNums: TArray<Integer>;
  AbundantSums: TDictionary<Integer, Boolean>; I, J, K, Sum: Integer;
BEGIN
  SetLength(SumOfDivisors, MAX_NUM + 1);
  FOR I := 1 TO MAX_NUM DO BEGIN
    SumOfDivisors[I] := 1;
    FOR J := 2 TO Round(Sqrt(I)) DO BEGIN
      IF I MOD J = 0 THEN BEGIN
        IF J = I DIV J THEN SumOfDivisors[I] := SumOfDivisors[I] + J
        ELSE SumOfDivisors[I] := SumOfDivisors[I] + J + I DIV J;
      END;
    END;
  END;

  SetLength(AbundantNums, 0);
  AbundantSums := TDictionary<Integer, Boolean>.Create;
  FOR I := 1 TO MAX_NUM DO BEGIN
    IF SumOfDivisors[I] > I THEN BEGIN
      SetLength(AbundantNums, Length(AbundantNums) + 1);
      AbundantNums[HIGH(AbundantNums)] := I;
    END;
  END;

  FOR I := 0 TO HIGH(AbundantNums) DO BEGIN
    FOR J := I TO HIGH(AbundantNums) DO BEGIN
      K := AbundantNums[I] + AbundantNums[J];
      IF K <= MAX_NUM THEN AbundantSums.AddOrSetValue(K, True);
    END;
  END;

  Sum := 0;
  FOR I := 1 TO MAX_NUM DO BEGIN
    IF NOT AbundantSums.ContainsKey(I) THEN Sum := Sum + I;
  END;

  AbundantSums.Free;
  Result := Sum;
END;

FUNCTION Problem024: STRING;
  PROCEDURE Swap(VAR A, B: Integer);
  VAR Temp: Integer;
  BEGIN
    Temp := A;
    A := B;
    B := Temp;
  END;
  FUNCTION NextPermute(VAR Nums: ARRAY OF Integer): Boolean;
  VAR I, J, N: Integer;
  BEGIN
    N := Length(Nums);
    I := N - 2;
    WHILE (I >= 0) AND (Nums[I] >= Nums[I + 1]) DO Dec(I);
    IF I < 0 THEN Exit(False);
    J := N - 1;
    WHILE (Nums[J] <= Nums[I]) DO Dec(J);
    Swap(Nums[I], Nums[J]);
    J := N - 1;
    Inc(I);
    WHILE (I < J) DO BEGIN
      Swap(Nums[I], Nums[J]);
      Inc(I);
      Dec(J);
    END;
    Result := True;
  END;

VAR Nums: ARRAY [0 .. 9] OF Integer; Perm: STRING; Count: Integer;
BEGIN
  FOR Count := 0 TO 9 DO Nums[Count] := Count;
  Count := 0;
  REPEAT
    Inc(Count);
    Perm := '';
    FOR VAR I := 0 TO HIGH(Nums) DO Perm := Perm + IntToStr(Nums[I]);
  UNTIL (Count = 1000000) OR NOT NextPermute(Nums);
  Result := Perm;
END;

FUNCTION Problem025: Integer;
VAR C: Integer; A, B, Fib, Limit: BigInteger;
BEGIN
  A := 1;
  B := 1;
  C := 2;
  Fib := BigInteger.One;
  Limit := BigInteger.Pow(10, 999);

  WHILE Fib < Limit DO BEGIN
    Fib := A + B;
    A := B;
    B := Fib;
    Inc(C);
  END;

  Result := C;
END;

FUNCTION Problem026: Integer;
VAR MaxCycleLength, D, CycleLength: Integer;
  Remainders: ARRAY [0 .. 999] OF Integer;
BEGIN
  MaxCycleLength := 0;
  Result := 0;

  FOR D := 2 TO 999 DO BEGIN
    CycleLength := 0;
    FillChar(Remainders, SizeOf(Remainders), 0);
    VAR
    Value := 1;
    WHILE (Remainders[Value] = 0) AND (Value <> 0) DO BEGIN
      Remainders[Value] := CycleLength;
      Value := (Value * 10) MOD D;
      Inc(CycleLength);
    END;
    IF CycleLength - Remainders[Value] > MaxCycleLength THEN BEGIN
      MaxCycleLength := CycleLength - Remainders[Value];
      Result := D;
    END;
  END;
END;

FUNCTION Problem027: Integer;

VAR A, B, N, MaxCount, MaxA, MaxB, Count: Integer; Primes: TArray<Boolean>;
BEGIN
  Primes := Sieve(1000000);
  // generate boolean array of primes up to 1,000,000
  MaxCount := 0;
  MaxA := 0;
  MaxB := 0;
  FOR A := -999 TO 999 DO
    FOR B := -1000 TO 1000 DO BEGIN
      Count := 0;
      N := 0;
      WHILE ((N * N + A * N + B) > 0) AND Primes[N * N + A * N + B] DO BEGIN
        Inc(Count);
        Inc(N);
      END;
      IF Count > MaxCount THEN BEGIN
        MaxCount := Count;
        MaxA := A;
        MaxB := B;
      END;
    END;
  Result := MaxA * MaxB;
END;

FUNCTION Problem028: Uint64;
VAR N, Size: Uint64;
BEGIN
  Result := 1;
  Size := 1001;
  N := 3;
  REPEAT
    Inc(Result, 4 * N * N - 6 * N + 6);
    Inc(N, 2)
  UNTIL N > Size;
END;

FUNCTION Problem029: Integer;
VAR A: BigInteger; I, J: Longint; Str: TStringList;

BEGIN
  Str := TStringList.Create;
  Str.Sorted := True;
  Str.Duplicates := DupIgnore;
  A := 1;
  FOR I := 2 TO 100 DO
    FOR J := 2 TO 100 DO BEGIN
      A := BigInteger.Pow(I, J);
      Str.Add(A.ToString);
    END;
  Result := Str.Count;
END;

FUNCTION Problem030: Integer;
CONST Power = 5;

VAR I, Som, X: Integer;
BEGIN
  Result := 0;
  FOR I := 2 TO (Power * Intpower(9, Power)) DO BEGIN
    Som := 0;
    FOR X := 1 TO Succ(Num_of_digits(I)) DO
        Som := Som + Intpower(Getdigit(I, X), Power);
    IF Som = I THEN Inc(Result, Som)
  END;
END;

END.
